use std::{
    fs::{File, OpenOptions, read_to_string},
    os::{
        fd::{AsRawFd, AsFd},
        unix::{io::OwnedFd, fs::OpenOptionsExt}
    },
    path::{Path, PathBuf},
    collections::HashMap,
    cmp::min,
    panic::{self, AssertUnwindSafe}
};
use cairo::{ImageSurface, Format, Context, Surface, Rectangle, FontSlant, FontWeight, FontFace, Antialias};
use rsvg::{Loader, CairoRenderer, SvgHandle};
use drm::control::ClipRect;
use anyhow::{Error, Result, anyhow};
use input::{
    Libinput, LibinputInterface, Device as InputDevice,
    event::{
        Event, device::DeviceEvent, EventTrait,
        touch::{TouchEvent, TouchEventPosition, TouchEventSlot},
        keyboard::{KeyboardEvent, KeyboardEventTrait, KeyState}
    }
};
use libc::{O_ACCMODE, O_RDONLY, O_RDWR, O_WRONLY, c_char};
use input_linux::{uinput::UInputHandle, EventKind, Key, SynchronizeKind};
use input_linux_sys::{uinput_setup, input_id, timeval, input_event};
use nix::{
    poll::{poll, PollFd, PollFlags},
    sys::{
        signal::{Signal, SigSet},
        inotify::{AddWatchFlags, InitFlags, Inotify, WatchDescriptor}
    },
    errno::Errno
};
use privdrop::PrivDrop;
use serde::Deserialize;
use freetype::Library as FtLibrary;
use icon_loader::{IconFileType, IconLoader};
use chrono::{Local, Locale, Timelike};

mod backlight;
mod display;
mod pixel_shift;
mod fonts;

use backlight::BacklightManager;
use display::DrmBackend;
use pixel_shift::{PixelShiftManager, PIXEL_SHIFT_WIDTH_PX};
use fonts::{FontConfig, Pattern};

const BUTTON_SPACING_PX: i32 = 16;
const BUTTON_COLOR_INACTIVE: f64 = 0.200;
const BUTTON_COLOR_ACTIVE: f64 = 0.400;
const ICON_SIZE: i32 = 48;
const TIMEOUT_MS: i32 = 10 * 1000;
const USER_CFG_PATH: &'static str = "/etc/tiny-dfr/config.toml";

#[derive(Deserialize)]
#[serde(rename_all = "PascalCase")]
struct ConfigProxy {
    media_layer_default: Option<bool>,
    special_extended_mode: Option<bool>,
    show_button_outlines: Option<bool>,
    enable_pixel_shift: Option<bool>,
    font_renderer: Option<String>,
    font_style: Option<String>,
    bold: Option<bool>,
    italic: Option<bool>,
    font_template: Option<String>,
    media_icon_theme: Option<String>,
    app_icon_theme: Option<String>,
    primary_layer_keys: Option<Vec<ButtonConfig>>,
    media_layer_keys: Option<Vec<ButtonConfig>>,
    app_layer_keys1: Option<Vec<ButtonConfig>>,
    app_layer_keys2: Option<Vec<ButtonConfig>>,
    app_layer_keys3: Option<Vec<ButtonConfig>>
}

#[derive(Deserialize)]
#[serde(rename_all = "PascalCase")]
struct ButtonConfig {
    #[serde(alias = "Svg")]
    icon: Option<String>,
    mode: Option<String>,
    text: Option<String>,
    format: Option<String>,
    locale: Option<String>,
    action: Key
}

struct Config {
    media_layer_default: bool,
    show_button_outlines: bool,
    enable_pixel_shift: bool,
    font_renderer: String,
    font_style_cairo: String,
    bold_cairo: bool,
    italic_cairo: bool,
    font_face: FontFace,
}

struct Theme {
    media_icon_theme: String,
    app_icon_theme: String
}

enum ButtonImage {
    Text(String),
    Svg(SvgHandle),
    Bitmap(ImageSurface),
    Time(String, String),
    Blank
}

struct Button {
    image: ButtonImage,
    changed: bool,
    active: bool,
    action: Key
}

fn load_image(path: &str, mode: Option<String>) -> Result<ButtonImage> {
    let theme = load_theme();
        let icon_theme = match mode {
            Some(mode_val) => {
                if mode_val == "App" {theme.app_icon_theme} else {theme.media_icon_theme}
            }
            None => {
                panic!("No mode specified")
            }
        };

    let mut search_paths: Vec<PathBuf> = vec![
        PathBuf::from("/etc/tiny-dfr/icons"),
        PathBuf::from("/usr/share/tiny-dfr/icons/"),
        PathBuf::from("/usr/share/icons/"),
    ];
    let mut loader = IconLoader::new();
    search_paths.extend(loader.search_paths().into_owned());
    loader.set_search_paths(search_paths);
    loader.set_theme_name_provider(icon_theme);
    loader.update_theme_name().unwrap();
    let icon_loader = match loader.load_icon(path) {
        Some(icon) => icon,
        None => return Err(anyhow!("Icon not found: {}, trying /usr/share/pixmaps", path)),
    };
    let icon = icon_loader.file_for_size(256);
    match icon.icon_type() {
        IconFileType::SVG => {
            let handle = Loader::new().read_path(icon.path())?;
            Ok(ButtonImage::Svg(handle))
        }
        IconFileType::PNG => {
            let mut file = File::open(icon.path())?;
            let surf = ImageSurface::create_from_png(&mut file)?;
            if surf.height() == ICON_SIZE && surf.width() == ICON_SIZE {
                return Ok(ButtonImage::Bitmap(surf));
            }
            let resized = ImageSurface::create(Format::ARgb32, ICON_SIZE, ICON_SIZE).unwrap();
            let c = Context::new(&resized).unwrap();
            c.scale(ICON_SIZE as f64 / surf.width() as f64, ICON_SIZE as f64 / surf.height() as f64);
            c.set_source_surface(surf, 0.0, 0.0).unwrap();
            c.set_antialias(Antialias::Best);
            c.paint().unwrap();
            return Ok(ButtonImage::Bitmap(resized));
        }
        IconFileType::XPM => {
            panic!("Legacy XPM icons are not supported")
        }
    }
}

fn try_load_svg_pixmap(path: &str) -> Result<ButtonImage> {
    let handle = Loader::new().read_path(format!("/usr/share/pixmaps/{}.svg", path))?;
    Ok(ButtonImage::Svg(handle))
}

fn try_load_png_pixmap(path: &str) -> Result<ButtonImage> {
    let mut file = File::open(format!("/usr/share/pixmaps/{}.png", path))?;
    let surf = ImageSurface::create_from_png(&mut file)?;
    if surf.height() == ICON_SIZE && surf.width() == ICON_SIZE {
        return Ok(ButtonImage::Bitmap(surf));
    }
    let resized = ImageSurface::create(Format::ARgb32, ICON_SIZE, ICON_SIZE).unwrap();
    let c = Context::new(&resized).unwrap();
    c.scale(ICON_SIZE as f64 / surf.width() as f64, ICON_SIZE as f64 / surf.height() as f64);
    c.set_source_surface(surf, 0.0, 0.0).unwrap();
    c.set_antialias(Antialias::Best);
    c.paint().unwrap();
    return Ok(ButtonImage::Bitmap(resized));
}

impl Button {
    fn with_config(cfg: ButtonConfig) -> Button {
        if let Some(text) = cfg.text {
            Button::new_text(text, cfg.action)
        } else if let Some(icon) = cfg.icon {
            Button::new_icon(&icon, cfg.action, cfg.mode)
        } else if let Some(mode) = cfg.mode {
            if mode.to_lowercase() == "blank" {
                Button::new_blank(cfg.action)
            } else if mode.to_lowercase() == "time" {
                let format = match cfg.format {
                    Some(f) => f,
                    None => "24hr".to_string()
                };
                let locale = match cfg.locale {
                    Some(l) => l,
                    None => "POSIX".to_string()
                };
                Button::new_time(cfg.action, format, locale)
            } else {
                panic!("Invalid config, a button must have either Text, Icon or be Blank")
            }
        } else {
            panic!("Invalid config, a button must have either Text, Icon or be Blank")
        }
    }
    fn new_text(text: String, action: Key) -> Button {
        Button {
            action,
            active: false,
            changed: false,
            image: ButtonImage::Text(text)
        }
    }
    fn new_icon(path: &str, action: Key, mode: Option<String>) -> Button {
        let image = load_image(path, mode)
            .or_else(|_| try_load_svg_pixmap(path))
            .or_else(|_| try_load_png_pixmap(path))
            .unwrap_or_else(|_| ButtonImage::Text(path.to_string()));
        Button {
            action, image,
            active: false,
            changed: false,
        }
    }
    fn new_time(action: Key, format: String, locale: String) -> Button {
        Button {
            action,
            active: false,
            changed: false,
            image: ButtonImage::Time(format, locale),
        }
    }
    fn new_blank(action: Key) -> Button {
        Button {
            action,
            active: false,
            changed: false,
            image: ButtonImage::Blank,
        }
    }
    fn render(&self, c: &Context, height: i32, button_left_edge: f64, button_width: u64, y_shift: f64) {
        match &self.image {
            ButtonImage::Text(text) => {
                let extents = c.text_extents(text).unwrap();
                c.move_to(
                    button_left_edge + (button_width as f64 / 2.0 - extents.width() / 2.0).round(),
                    y_shift + (height as f64 / 2.0 + extents.height() / 2.0).round()
                );
                c.show_text(text).unwrap();
            },
            ButtonImage::Svg(svg) => {
                let renderer = CairoRenderer::new(&svg);
                let x = button_left_edge + (button_width as f64 / 2.0 - (ICON_SIZE / 2) as f64).round();
                let y = y_shift + ((height as f64 - ICON_SIZE as f64) / 2.0).round();

                renderer.render_document(c,
                    &Rectangle::new(x, y, ICON_SIZE as f64, ICON_SIZE as f64)
                ).unwrap();
            }
            ButtonImage::Bitmap(surf) => {
                let x = button_left_edge + (button_width as f64 / 2.0 - (ICON_SIZE / 2) as f64).round();
                let y = y_shift + ((height as f64 - ICON_SIZE as f64) / 2.0).round();
                c.set_source_surface(surf, x, y).unwrap();
                c.rectangle(x, y, ICON_SIZE as f64, ICON_SIZE as f64);
                c.fill().unwrap();
            }
            ButtonImage::Time(format, locale) => {
                let current_time = Local::now();
                let current_locale = Locale::try_from(locale.as_str()).unwrap_or(Locale::POSIX);
                let formatted_time;
                if format == "24hr" {
                    formatted_time = format!(
                    "{}:{}    {} {} {}",
                     current_time.format_localized("%H", current_locale),
                     current_time.format_localized("%M", current_locale),
                     current_time.format_localized("%a", current_locale),
                     current_time.format_localized("%-e", current_locale),
                     current_time.format_localized("%b", current_locale)
                );
                } else {
                    formatted_time = format!(
                    "{}:{} {}    {} {} {}",
                    current_time.format_localized("%-l", current_locale),
                    current_time.format_localized("%M", current_locale),
                    current_time.format_localized("%p", current_locale),
                    current_time.format_localized("%a", current_locale),
                    current_time.format_localized("%-e", current_locale),
                    current_time.format_localized("%b", current_locale)
                );
                }
                let time_extents = c.text_extents(&formatted_time).unwrap();
                c.move_to(
                    button_left_edge + (button_width as f64 / 2.0 - time_extents.width() / 2.0).round(),
                    y_shift + (height as f64 / 2.0 + time_extents.height() / 2.0).round()
                );
                c.show_text(&formatted_time).unwrap();
            }
            _ => {
            }
        }
    }
    fn set_active<F>(&mut self, uinput: &mut UInputHandle<F>, active: bool) where F: AsRawFd {
        if self.active != active {
            self.active = active;
            self.changed = true;

            toggle_key(uinput, self.action, active as i32);
        }
    }
}

#[derive(Default)]
struct FunctionLayer {
    buttons: Vec<Button>
}

impl FunctionLayer {
    fn with_config(cfg: Vec<ButtonConfig>) -> FunctionLayer {
        if cfg.is_empty() {
            panic!("Invalid configuration, layer has 0 buttons");
        }
        FunctionLayer {
            buttons: cfg.into_iter().map(Button::with_config).collect()
        }
    }
    fn draw(&mut self, config: &Config, width: i32, height: i32, surface: &Surface, pixel_shift: (f64, f64), complete_redraw: bool) -> Vec<ClipRect> {
        let c = Context::new(&surface).unwrap();
        let mut modified_regions = if complete_redraw {
            vec![ClipRect::new(0, 0, height as u16, width as u16)]
        } else {
            Vec::new()
        };
        c.translate(height as f64, 0.0);
        c.rotate((90.0f64).to_radians());
        let pixel_shift_width = if config.enable_pixel_shift { PIXEL_SHIFT_WIDTH_PX } else { 0 };
        let button_width = ((width - pixel_shift_width as i32) - (BUTTON_SPACING_PX * (self.buttons.len() - 1) as i32)) as f64 / self.buttons.len() as f64;
        let radius = 8.0f64;
        let bot = (height as f64) * 0.15;
        let top = (height as f64) * 0.85;
        let (pixel_shift_x, pixel_shift_y) = pixel_shift;

        if complete_redraw {
            c.set_source_rgb(0.0, 0.0, 0.0);
            c.paint().unwrap();
        }
        if config.font_renderer.to_lowercase() == "cairo" {
            c.select_font_face(&config.font_style_cairo, if config.italic_cairo {FontSlant::Italic} else {FontSlant::Normal}, if config.bold_cairo {FontWeight::Bold} else {FontWeight::Normal});
        } else if config.font_renderer.to_lowercase() == "freetype" {
            c.set_font_face(&config.font_face);
        } else { panic!("Invalid font renderer chosen. Choose between \"Cairo\" and \"FreeType\""); }
        c.set_font_size(32.0);
        for (i, button) in self.buttons.iter_mut().enumerate() {
            if !button.changed && !complete_redraw {
                continue;
            };

            let left_edge = (i as f64 * (button_width + BUTTON_SPACING_PX as f64)).floor() + pixel_shift_x + (pixel_shift_width / 2) as f64;
            let color = if button.active {
                BUTTON_COLOR_ACTIVE
            } else if config.show_button_outlines {
                BUTTON_COLOR_INACTIVE
            } else {
                0.0
            };
            if !complete_redraw {
                c.set_source_rgb(0.0, 0.0, 0.0);
                if button.action == Key::Time {
                    c.rectangle(left_edge, bot - radius, button_width * 3.0, top - bot + radius * 2.0);
                } else {
                    c.rectangle(left_edge, bot - radius, button_width, top - bot + radius * 2.0);
                }
                c.fill().unwrap();
            }

            if (button.action != Key::Unknown &&
               button.action != Key::Time &&
               button.action != Key::Macro1 &&
               button.action != Key::Macro2 &&
               button.action != Key::Macro3 &&
               button.action != Key::Macro4) &&
               ((button.action != Key::WWW &&
                button.action != Key::AllApplications &&
                button.action != Key::Calc &&
                button.action != Key::File &&
                button.action != Key::Prog1 &&
                button.action != Key::Prog2 &&
                button.action != Key::Prog3 &&
                button.action != Key::Prog4) ||
                button.active) {
            c.set_source_rgb(color, color, color);
            // draw box with rounded corners
            c.new_sub_path();
            let left = left_edge + radius;
            let right = (left_edge + button_width.ceil()) - radius;
            c.arc(
                right,
                bot,
                radius,
                (-90.0f64).to_radians(),
                (0.0f64).to_radians(),
            );
            c.arc(
                right,
                top,
                radius,
                (0.0f64).to_radians(),
                (90.0f64).to_radians(),
            );
            c.arc(
                left,
                top,
                radius,
                (90.0f64).to_radians(),
                (180.0f64).to_radians(),
            );
            c.arc(
                left,
                bot,
                radius,
                (180.0f64).to_radians(),
                (270.0f64).to_radians(),
            );
            c.close_path();

            c.fill().unwrap();
            }
            c.set_source_rgb(1.0, 1.0, 1.0);
            if button.action == Key::Time {
                button.render(&c, height, left_edge, button_width.ceil() as u64 * 3, pixel_shift_y);
            } else {
                button.render(&c, height, left_edge, button_width.ceil() as u64, pixel_shift_y);
            }

            button.changed = false;

            if !complete_redraw {
                if button.action == Key::Time {
                    modified_regions.push(ClipRect::new(
                        height as u16 - top as u16 - radius as u16,
                        left_edge as u16,
                        height as u16 - bot as u16 + radius as u16,
                        left_edge as u16 + button_width as u16 * 3
                    ));
                } else {
                    modified_regions.push(ClipRect::new(
                        height as u16 - top as u16 - radius as u16,
                        left_edge as u16,
                        height as u16 - bot as u16 + radius as u16,
                        left_edge as u16 + button_width as u16
                    ));
                }
            }
        }

        modified_regions
    }
}

struct Interface;

impl LibinputInterface for Interface {
    fn open_restricted(&mut self, path: &Path, flags: i32) -> Result<OwnedFd, i32> {
        let mode = flags & O_ACCMODE;

        OpenOptions::new()
            .custom_flags(flags)
            .read(mode == O_RDONLY || mode == O_RDWR)
            .write(mode == O_WRONLY || mode == O_RDWR)
            .open(path)
            .map(|file| file.into())
            .map_err(|err| err.raw_os_error().unwrap())
    }
    fn close_restricted(&mut self, fd: OwnedFd) {
        _ = File::from(fd);
    }
}


fn button_hit(num: u32, idx: u32, width: u16, height: u16, x: f64, y: f64) -> bool {
    let button_width = (width as i32 - (BUTTON_SPACING_PX * (num - 1) as i32)) as f64 / num as f64;
    let left_edge = idx as f64 * (button_width + BUTTON_SPACING_PX as f64);
    if x < left_edge || x > (left_edge + button_width) {
        return false
    }
    y > 0.1 * height as f64 && y < 0.9 * height as f64
}

fn emit<F>(uinput: &mut UInputHandle<F>, ty: EventKind, code: u16, value: i32) where F: AsRawFd {
    uinput.write(&[input_event {
        value: value,
        type_: ty as u16,
        code: code,
        time: timeval {
            tv_sec: 0,
            tv_usec: 0
        }
    }]).unwrap();
}

fn toggle_key<F>(uinput: &mut UInputHandle<F>, code: Key, value: i32) where F: AsRawFd {
    emit(uinput, EventKind::Key, code as u16, value);
    emit(uinput, EventKind::Synchronize, SynchronizeKind::Report as u16, 0);
}

fn load_font(name: &str) -> FontFace {
    let fontconfig = FontConfig::new();
    let mut pattern = Pattern::new(name);
    fontconfig.perform_substitutions(&mut pattern);
    let pat_match = fontconfig.match_pattern(&pattern);
    let file_name = pat_match.get_file_name();
    let file_idx = pat_match.get_font_index();
    let ft_library = FtLibrary::init().unwrap();
    let face = ft_library.new_face(file_name, file_idx).unwrap();
    FontFace::create_from_ft(&face).unwrap()
}

fn load_theme() -> Theme {
    let mut base = toml::from_str::<ConfigProxy>(&read_to_string("/usr/share/tiny-dfr/config.toml").unwrap()).unwrap();
    let user = read_to_string("/etc/tiny-dfr/config.toml").map_err::<Error, _>(|e| e.into())
        .and_then(|r| Ok(toml::from_str::<ConfigProxy>(&r)?));
    if let Ok(user) = user {
        base.media_icon_theme = user.media_icon_theme.or(base.media_icon_theme);
        base.app_icon_theme = user.app_icon_theme.or(base.app_icon_theme);
    };
    Theme {
        media_icon_theme: base.media_icon_theme.unwrap(),
        app_icon_theme: base.app_icon_theme.unwrap()
    }
}

fn load_config(width: u16) -> (Config, Vec<FunctionLayer>) {
    let mut base = toml::from_str::<ConfigProxy>(&read_to_string("/usr/share/tiny-dfr/config.toml").unwrap()).unwrap();
    let user = read_to_string(USER_CFG_PATH).map_err::<Error, _>(|e| e.into())
        .and_then(|r| Ok(toml::from_str::<ConfigProxy>(&r)?));
    if let Ok(user) = user {
        base.media_layer_default = user.media_layer_default.or(base.media_layer_default);
        base.special_extended_mode = user.special_extended_mode.or(base.special_extended_mode);
        base.show_button_outlines = user.show_button_outlines.or(base.show_button_outlines);
        base.enable_pixel_shift = user.enable_pixel_shift.or(base.enable_pixel_shift);
        base.font_renderer = user.font_renderer.or(base.font_renderer);
        base.font_style = user.font_style.or(base.font_style);
        base.bold = user.bold.or(base.bold);
        base.italic = user.italic.or(base.italic);
        base.font_template = user.font_template.or(base.font_template);
        base.media_layer_keys = user.media_layer_keys.or(base.media_layer_keys);
        base.primary_layer_keys = user.primary_layer_keys.or(base.primary_layer_keys);
        base.app_layer_keys1 = user.app_layer_keys1.or(base.app_layer_keys1);
        base.app_layer_keys2 = user.app_layer_keys2.or(base.app_layer_keys2);
        base.app_layer_keys3 = user.app_layer_keys3.or(base.app_layer_keys3);
    };
    let media_layer = FunctionLayer::with_config(base.media_layer_keys.unwrap());
    let fkey_layer = FunctionLayer::with_config(base.primary_layer_keys.unwrap());
    let app_layer1 = FunctionLayer::with_config(base.app_layer_keys1.unwrap());
    let app_layer2 = FunctionLayer::with_config(base.app_layer_keys2.unwrap());
    let app_layer3 = FunctionLayer::with_config(base.app_layer_keys3.unwrap());
    let mut layers = if base.media_layer_default.unwrap() {
            if base.special_extended_mode.unwrap() {
                vec![app_layer1, fkey_layer, app_layer2, app_layer3]
            } else {
                vec![media_layer, fkey_layer]
            }
        } else {
            if base.special_extended_mode.unwrap() {
                vec![fkey_layer, app_layer1, app_layer2, app_layer3]
            } else {
                vec![fkey_layer, media_layer]
            }
        };

    if width >= 2170 {
        for layer in &mut layers {
            layer.buttons.insert(0, Button::new_text("esc".to_string(), Key::Esc));
        }
    }

    let cfg = Config {
        media_layer_default: base.media_layer_default.unwrap(),
        show_button_outlines: base.show_button_outlines.unwrap(),
        enable_pixel_shift: base.enable_pixel_shift.unwrap(),
        font_renderer: base.font_renderer.unwrap(),
        font_style_cairo: base.font_style.unwrap(),
        bold_cairo: base.bold.unwrap(),
        italic_cairo: base.italic.unwrap(),
        font_face: load_font(&base.font_template.unwrap()),
    };
    (cfg, layers)
}

fn main() {
    let mut drm = DrmBackend::open_card().unwrap();
    let (height, width) = drm.mode().size();
    let _ = panic::catch_unwind(AssertUnwindSafe(|| {
        real_main(&mut drm)
    }));
    let crash_bitmap = include_bytes!("crash_bitmap.raw");
    let mut map = drm.map().unwrap();
    let data = map.as_mut();
    let mut wptr = 0;
    for byte in crash_bitmap {
        for i in 0..8 {
            let bit = ((byte >> i) & 0x1) == 0;
            let color = if bit { 0xFF } else { 0x0 };
            data[wptr] = color;
            data[wptr + 1] = color;
            data[wptr + 2] = color;
            data[wptr + 3] = color;
            wptr += 4;
        }
    }
    drop(map);
    drm.dirty(&[ClipRect::new(0, 0, height as u16, width as u16)]).unwrap();
    let mut sigset = SigSet::empty();
    sigset.add(Signal::SIGTERM);
    sigset.wait().unwrap();
}

fn arm_inotify(inotify_fd: &Inotify) -> WatchDescriptor {
    let flags = AddWatchFlags::IN_MOVED_TO | AddWatchFlags::IN_CLOSE | AddWatchFlags::IN_ONESHOT;
    inotify_fd.add_watch(USER_CFG_PATH, flags).unwrap()
}

fn real_main(drm: &mut DrmBackend) {
    let (height, width) = drm.mode().size();
    let (db_width, db_height) = drm.fb_info().unwrap().size();
    let mut uinput = UInputHandle::new(OpenOptions::new().write(true).open("/dev/uinput").unwrap());
    let mut backlight = BacklightManager::new();
    let mut last_redraw_minute = Local::now().minute();
    let (mut cfg, mut layers) = load_config(width);
    let mut pixel_shift = PixelShiftManager::new();

    // drop privileges to input and video group
    let groups = ["input", "video"];

    PrivDrop::default()
        .user("nobody")
        .group_list(&groups)
        .apply()
        .unwrap_or_else(|e| { panic!("Failed to drop privileges: {}", e) });

    let mut surface = ImageSurface::create(Format::ARgb32, db_width as i32, db_height as i32).unwrap();
    let mut active_layer = 0;
    let mut needs_complete_redraw = true;

    let mut input_tb = Libinput::new_with_udev(Interface);
    let mut input_main = Libinput::new_with_udev(Interface);
    input_tb.udev_assign_seat("seat-touchbar").unwrap();
    input_main.udev_assign_seat("seat0").unwrap();
    let fd_tb = input_tb.as_fd().try_clone_to_owned().unwrap();
    let fd_main = input_main.as_fd().try_clone_to_owned().unwrap();
    let pollfd_tb = PollFd::new(&fd_tb, PollFlags::POLLIN);
    let pollfd_main = PollFd::new(&fd_main, PollFlags::POLLIN);
    uinput.set_evbit(EventKind::Key).unwrap();
    for layer in &layers {
        for button in &layer.buttons {
            uinput.set_keybit(button.action).unwrap();
        }
    }
    let inotify_fd = Inotify::init(InitFlags::IN_NONBLOCK).unwrap();
    let mut cfg_watch_desc = arm_inotify(&inotify_fd);
    let pollfd_notify = PollFd::new(&inotify_fd, PollFlags::POLLIN);
    let mut dev_name_c = [0 as c_char; 80];
    let dev_name = "Dynamic Function Row Virtual Input Device".as_bytes();
    for i in 0..dev_name.len() {
        dev_name_c[i] = dev_name[i] as c_char;
    }
    uinput.dev_setup(&uinput_setup {
        id: input_id {
            bustype: 0x19,
            vendor: 0x1209,
            product: 0x316E,
            version: 1
        },
        ff_effects_max: 0,
        name: dev_name_c
    }).unwrap();
    uinput.dev_create().unwrap();

    let mut digitizer: Option<InputDevice> = None;
    let mut touches = HashMap::new();
    loop {
        let evts = match inotify_fd.read_events() {
            Ok(e) => e,
            Err(Errno::EAGAIN) => Vec::new(),
            r => r.unwrap(),
        };
        for evt in evts {
            if evt.wd != cfg_watch_desc {
                continue
            }
            (cfg, layers) = load_config(width);
            active_layer = 0;
            needs_complete_redraw = true;
            cfg_watch_desc = arm_inotify(&inotify_fd);
        }

        let mut next_timeout_ms = TIMEOUT_MS;
        if cfg.enable_pixel_shift {
            let (pixel_shift_needs_redraw, pixel_shift_next_timeout_ms) = pixel_shift.update();
            if pixel_shift_needs_redraw {
                needs_complete_redraw = true;
            }
            next_timeout_ms = min(next_timeout_ms, pixel_shift_next_timeout_ms);
        }

        let current_minute = Local::now().minute();
	for button in &mut layers[active_layer].buttons {
    	    if (button.action == Key::Time) && (current_minute != last_redraw_minute) {
                needs_complete_redraw = true;
                last_redraw_minute = current_minute;
    	    }
    	}

        if needs_complete_redraw || layers[active_layer].buttons.iter().any(|b| b.changed) {
            let shift = if cfg.enable_pixel_shift {
                pixel_shift.get()
            } else {
                (0.0, 0.0)
            };
            let clips = layers[active_layer].draw(&cfg, width as i32, height as i32, &surface, shift, needs_complete_redraw);
            let data = surface.data().unwrap();
            drm.map().unwrap().as_mut()[..data.len()].copy_from_slice(&data);
            drm.dirty(&clips).unwrap();
            needs_complete_redraw = false;
        }

        poll(&mut [pollfd_tb, pollfd_main, pollfd_notify], next_timeout_ms).unwrap();
        input_tb.dispatch().unwrap();
        input_main.dispatch().unwrap();
        for event in &mut input_tb.clone().chain(input_main.clone()) {
            backlight.process_event(&event);
            match event {
                Event::Device(DeviceEvent::Added(evt)) => {
                    let dev = evt.device();
                    if dev.name().contains(" Touch Bar") {
                        digitizer = Some(dev);
                    }
                },
                Event::Keyboard(KeyboardEvent::Key(key)) => {
                    if key.key() == Key::Fn as u32 {
                        let new_layer = match key.key_state() {
                            KeyState::Pressed => 1,
                            KeyState::Released => 0
                        };
                        if active_layer != new_layer {
                            active_layer = new_layer;
                            needs_complete_redraw = true;
                        }
                        } else if key.key() == Key::Macro1 as u32 && key.key_state() == KeyState::Pressed {
                            if cfg.media_layer_default {active_layer = 0;} else {active_layer = 1;}
                            needs_complete_redraw = true;
                        } else if key.key() == Key::Macro2 as u32 && key.key_state() == KeyState::Pressed {
                            active_layer = 2;
                            needs_complete_redraw = true;
                        } else if key.key() == Key::Macro3 as u32 && key.key_state() == KeyState::Pressed {
                            active_layer = 3;
                            needs_complete_redraw = true;
                    }
                },
                Event::Touch(te) => {
                    if Some(te.device()) != digitizer || backlight.current_bl() == 0 {
                        continue
                    }
                    match te {
                        TouchEvent::Down(dn) => {
                            let x = dn.x_transformed(width as u32);
                            let y = dn.y_transformed(height as u32);
                            let btn = (x / (width as f64 / layers[active_layer].buttons.len() as f64)) as u32;
                            if button_hit(layers[active_layer].buttons.len() as u32, btn, width, height, x, y) {
                                let button = &mut layers[active_layer].buttons[btn as usize];
                                if button.action == Key::Unknown || button.action == Key::Time {
                                    continue;
                                }
                                touches.insert(dn.seat_slot(), (active_layer, btn));
                                layers[active_layer].buttons[btn as usize].set_active(&mut uinput, true);
                            }
                        },
                        TouchEvent::Motion(mtn) => {
                            if !touches.contains_key(&mtn.seat_slot()) {
                                continue;
                            }

                            let x = mtn.x_transformed(width as u32);
                            let y = mtn.y_transformed(height as u32);
                            let (layer, btn) = *touches.get(&mtn.seat_slot()).unwrap();
                            let hit = button_hit(layers[layer].buttons.len() as u32, btn, width, height, x, y);
                            let button = &mut layers[layer].buttons[btn as usize];
                            if button.action == Key::Unknown || button.action == Key::Time {
                                continue;
                            }
                            button.set_active(&mut uinput, hit);
                        },
                        TouchEvent::Up(up) => {
                            if !touches.contains_key(&up.seat_slot()) {
                                continue;
                            }
                            let (layer, btn) = *touches.get(&up.seat_slot()).unwrap();
                            let button = &mut layers[layer].buttons[btn as usize];
                            if button.action == Key::Unknown || button.action == Key::Time {
                                continue;
                            }
                            button.set_active(&mut uinput, false);
                        }
                        _ => {}
                    }
                },
                _ => {}
            }
        }
        backlight.update_backlight();
    }
}
