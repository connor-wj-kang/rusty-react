use wasm_bindgen::UnwrapThrowExt;
use web_sys::{
    js_sys::{Object, Reflect},
    wasm_bindgen::JsValue,
};

thread_local! {
    pub static WINDOW: web_sys::Window = web_sys::window().unwrap_throw();
    pub static DOCUMENT: web_sys::Document = web_sys::window().unwrap_throw().document().unwrap_throw();
}

pub fn window() -> web_sys::Window {
    WINDOW.with(Clone::clone)
}

pub fn document() -> web_sys::Document {
    DOCUMENT.with(Clone::clone)
}

pub fn get_property<Target, Key>(target: &Target, key: Key) -> JsValue
where
    Target: Into<Object> + web_sys::wasm_bindgen::JsCast,
    Key: Into<JsValue>,
{
    if let Ok(value) = Reflect::get(&target.into(), &key.into()) {
        value
    } else {
        JsValue::UNDEFINED
    }
}

pub fn set_property<Target, Key, Value>(target: &Target, key: Key, value: Value)
where
    Target: Into<Object> + web_sys::wasm_bindgen::JsCast,
    Key: Into<JsValue>,
    Value: Into<JsValue>,
{
    Reflect::set(&target.into(), &key.into(), &value.into()).expect("target is not a object");
}
pub fn has_property<Target, Key>(target: &Target, key: Key) -> bool
where
    Target: Into<Object> + web_sys::wasm_bindgen::JsCast,
    Key: Into<JsValue>,
{
    Reflect::has(&target.into(), &key.into()).unwrap()
}
