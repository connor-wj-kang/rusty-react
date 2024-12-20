mod utils;

use js_sys::{Array, Function, JsString, Object};
use std::{
    cell::{Cell, RefCell},
    panic,
    rc::Rc,
};
use utils::{document, get_property, has_property, set_property, window};
use wasm_bindgen::prelude::*;
use web_sys::{IdleDeadline, Node};

thread_local! {
    pub static WORK_IN_PROGRESS: RefCell<Option<FiberMutRef>> = RefCell::new(None);
    pub static WORK_IN_PROGRESS_ROOT: RefCell<Option<FiberMutRef>> = RefCell::new(None);
    pub static CURRENT_ROOT: RefCell<Option<FiberMutRef>> = RefCell::new(None);
    pub static NEXT_UNIT_OF_WORK: RefCell<Option<FiberMutRef>> = RefCell::new(None);
    pub static HOOK_INDEX: Cell<usize> = Cell::new(0);
    pub static DELETIONS: RefCell<Vec<FiberMutRef>> = RefCell::new(Vec::new());
    pub static WORK_LOOP: JsValue = Closure::wrap(Box::new(work_loop) as Box<dyn Fn(IdleDeadline)>).into_js_value();
    pub static CURRENT_HOOK: RefCell<Object> = RefCell::new(Object::new());
}

#[wasm_bindgen(js_name = createElement)]
pub fn create_element(r#type: &str, props: &Object, children: Array) -> Object {
    let react_element = Object::new();
    set_property(&react_element, "type", r#type);
    let new_props = Object::new();
    Object::get_own_property_names(props)
        .iter()
        .for_each(|key| {
            if key.as_string().unwrap() != "children" {
                set_property(&new_props, &key, get_property(props, &key));
            }
        });
    let children = children
        .iter()
        .map(|child| {
            if child.is_object() {
                child
            } else {
                create_text_element(child.as_string().unwrap().as_str()).unchecked_into::<JsValue>()
            }
        })
        .collect::<Vec<JsValue>>();
    set_property(&new_props, "children", children);
    set_property(&react_element, "props", new_props);
    react_element
}

pub fn create_text_element(text: &str) -> Object {
    let text_element = Object::new();
    set_property(&text_element, "type", "TEXT_ELEMENT");
    let new_props = Object::new();
    set_property(&new_props, "nodeValue", text);
    set_property(&new_props, "children", Array::new());
    set_property(&text_element, "props", new_props);
    text_element
}

pub fn create_dom_element(fiber: FiberMutRef) -> Node {
    let fiber = fiber.borrow();
    let dom: Node = if fiber.r#type == "TEXT_ELEMENT" {
        document().create_text_node("").unchecked_into()
    } else {
        document()
            .create_element(&fiber.r#type.as_string().unwrap())
            .unwrap()
            .unchecked_into()
    };
    update_dom(&dom, &Object::new(), &fiber.props);
    dom
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum EffectTag {
    #[default]
    NoFlag,
    Update,
    Deletion,
    Placement,
}

type FiberMutRef = Rc<RefCell<Fiber>>;

pub struct Fiber {
    r#type: JsValue,
    props: Object,
    dom: Option<Node>,
    parent: Option<FiberMutRef>,
    sibling: Option<FiberMutRef>,
    child: Option<FiberMutRef>,
    alternate: Option<FiberMutRef>,
    effect_tag: EffectTag,
    hooks: Vec<JsValue>,
}

impl Default for Fiber {
    fn default() -> Self {
        Self {
            r#type: Default::default(),
            props: Default::default(),
            dom: Default::default(),
            parent: Default::default(),
            sibling: Default::default(),
            child: Default::default(),
            alternate: Default::default(),
            effect_tag: Default::default(),
            hooks: Default::default(),
        }
    }
}

#[wasm_bindgen]
pub fn render(element: JsValue, container: Node) {
    let props = Object::new();
    set_property(&props, "children", vec![element]);
    WORK_IN_PROGRESS_ROOT.replace(Some(Rc::new(RefCell::new(Fiber {
        dom: Some(container),
        props,
        alternate: CURRENT_ROOT.with_borrow(|current_root| current_root.clone()),
        ..Default::default()
    }))));
    DELETIONS.replace(Vec::new());
    NEXT_UNIT_OF_WORK.replace(WORK_IN_PROGRESS_ROOT.with_borrow(|wip_root| wip_root.clone()));
}

pub fn update_dom(dom: &Node, prev_props: &Object, next_props: &Object) {
    let is_event = |key: &JsValue| -> bool { key.unchecked_ref::<JsString>().starts_with("on", 0) };
    let is_property =
        |key: &JsValue| -> bool { key.unchecked_ref::<JsString>() != "children" && !is_event(key) };
    let is_new = |key: &JsValue| -> bool {
        let prev_value = get_property(prev_props, key);
        let next_value = get_property(next_props, key);
        prev_value != next_value
    };
    let is_gone = |key: &JsValue| -> bool { !has_property(next_props, key) };
    Object::keys(prev_props)
        .iter()
        .filter(is_event)
        .filter(|key| !has_property(next_props, key) || is_new(key))
        .for_each(|name| {
            let event_type = &name.as_string().unwrap().to_lowercase()[2..];
            dom.remove_event_listener_with_callback(
                event_type,
                get_property(prev_props, name).unchecked_ref(),
            )
            .unwrap();
        });
    Object::keys(prev_props)
        .iter()
        .filter(is_property)
        .filter(is_gone)
        .for_each(|name| set_property(dom, name, ""));
    Object::keys(next_props)
        .iter()
        .filter(is_property)
        .filter(is_new)
        .for_each(|name| set_property(dom, &name, get_property(next_props, &name)));
    Object::keys(next_props)
        .iter()
        .filter(is_event)
        .filter(is_new)
        .for_each(|name| {
            let event_type = &name.as_string().unwrap().to_lowercase()[2..];
            dom.add_event_listener_with_callback(
                event_type,
                get_property(next_props, name).unchecked_ref(),
            )
            .unwrap();
        });
}

fn commit_root() {
    DELETIONS.with_borrow(|deletions| {
        deletions
            .iter()
            .for_each(|work| commit_work(Some(work.clone())))
    });
    WORK_IN_PROGRESS_ROOT
        .with_borrow(|wip_root| commit_work(wip_root.clone().unwrap().borrow().child.clone()));
    CURRENT_ROOT.replace(WORK_IN_PROGRESS_ROOT.take());
}

fn commit_work(fiber: Option<FiberMutRef>) {
    if fiber.is_none() {
        return;
    }
    let binding = fiber.unwrap();
    let fiber = binding.borrow();
    let mut dom_parent_fiber = fiber.parent.clone();
    while dom_parent_fiber
        .clone()
        .is_some_and(|fiber| fiber.borrow().dom.is_none())
    {
        dom_parent_fiber = dom_parent_fiber.unwrap().borrow().parent.clone();
    }
    let dom_parent = dom_parent_fiber.unwrap().borrow().dom.clone().unwrap();
    if fiber.effect_tag == EffectTag::Placement && fiber.dom.is_some() {
        dom_parent
            .append_child(fiber.dom.as_ref().unwrap())
            .unwrap();
    } else if fiber.effect_tag == EffectTag::Update && fiber.dom.is_some() {
        update_dom(
            fiber.dom.as_ref().unwrap(),
            &fiber.alternate.clone().unwrap().borrow().props,
            &fiber.props,
        );
    } else if fiber.effect_tag == EffectTag::Deletion {
        commit_deletion(binding.clone(), &dom_parent);
    }
    commit_work(fiber.child.clone());
    commit_work(fiber.sibling.clone());
}

fn commit_deletion(fiber: FiberMutRef, dom_parent: &Node) {
    if let Some(dom) = &fiber.borrow().dom {
        dom_parent.remove_child(dom).unwrap();
    } else {
        commit_deletion(fiber.borrow().child.clone().unwrap().clone(), dom_parent);
    }
}

#[wasm_bindgen]
pub fn start_work_loop() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    WORK_LOOP.with(|work_loop| {
        window()
            .request_idle_callback(work_loop.as_ref().unchecked_ref())
            .unwrap();
    });
}

pub fn work_loop(deadline: IdleDeadline) {
    let mut should_yield = false;
    while NEXT_UNIT_OF_WORK.with(|inner| inner.borrow().is_some()) && !should_yield {
        NEXT_UNIT_OF_WORK.with_borrow_mut(|inner| *inner = perform_unit_of_work(inner.clone()));
        should_yield = deadline.time_remaining() < 1.0;
    }
    if NEXT_UNIT_OF_WORK.with(|inner| inner.borrow().is_none())
        && WORK_IN_PROGRESS_ROOT.with(|wip_root| wip_root.borrow().is_some())
    {
        commit_root();
    }
    start_work_loop();
}

fn perform_unit_of_work(fiber: Option<FiberMutRef>) -> Option<FiberMutRef> {
    let is_function_component = fiber.clone().unwrap().borrow().r#type.is_function();
    if is_function_component {
        update_function_component(fiber.clone().unwrap());
    } else {
        update_host_component(fiber.clone().unwrap());
    }
    if fiber.clone().unwrap().borrow().child.is_some() {
        return fiber.clone().unwrap().borrow().child.clone();
    }
    let mut next_fiber = fiber.clone();
    while let Some(fiber) = next_fiber {
        if fiber.borrow().sibling.is_some() {
            return fiber.borrow().sibling.clone();
        }
        next_fiber = fiber.borrow().parent.clone();
    }
    None
}

fn update_function_component(work_in_progress: FiberMutRef) {
    WORK_IN_PROGRESS.replace(Some(work_in_progress.clone()));
    HOOK_INDEX.replace(0);
    WORK_IN_PROGRESS.with_borrow_mut(|wip| wip.clone().unwrap().borrow_mut().hooks = Vec::new());
    let children = vec![work_in_progress
        .borrow()
        .r#type
        .unchecked_ref::<Function>()
        .call1(&JsValue::undefined(), &work_in_progress.borrow().props)
        .unwrap()];
    reconciler_children(work_in_progress, children);
}

fn update_host_component(work_in_progress: FiberMutRef) {
    if work_in_progress.borrow().dom.is_none() {
        work_in_progress.borrow_mut().dom = Some(create_dom_element(work_in_progress.clone()));
    }
    reconciler_children(
        work_in_progress.clone(),
        get_property(&work_in_progress.borrow().props, "children")
            .unchecked_ref::<Array>()
            .iter()
            .collect(),
    );
}

#[wasm_bindgen(js_name = useState)]
pub fn use_state(initial: JsValue) -> Vec<JsValue> {
    let old_hook = WORK_IN_PROGRESS.with_borrow(|wip| match wip {
        Some(wip) => match &wip.borrow().alternate {
            Some(alternate) => {
                if alternate.clone().borrow().hooks.len() == 0 {
                    None
                } else {
                    alternate
                        .clone()
                        .borrow()
                        .hooks
                        .get(HOOK_INDEX.get())
                        .cloned()
                }
            }
            None => None,
        },
        None => None,
    });
    let hook = Object::new();
    set_property(
        &hook,
        "state",
        match &old_hook {
            Some(old_hook) => get_property(old_hook, "state"),
            None => initial,
        },
    );
    set_property(&hook, "queue", Array::new());
    let actions = match &old_hook {
        Some(hook) => get_property(hook, "queue").unchecked_into::<Array>(),
        None => Array::new(),
    };
    actions.iter().for_each(|action| {
        set_property(
            &hook,
            "state",
            action
                .unchecked_ref::<Function>()
                .call1(&JsValue::undefined(), &get_property(&hook, "state"))
                .unwrap(),
        )
    });
    WORK_IN_PROGRESS.with_borrow_mut(|wip| unsafe {
        wip.clone()
            .unwrap()
            .as_ptr()
            .as_mut()
            .unwrap()
            .hooks
            .push(hook.clone().into());
    });
    HOOK_INDEX.set(HOOK_INDEX.get() + 1);
    CURRENT_HOOK.replace(hook.clone());
    let set_state = |action: JsValue| {
        CURRENT_HOOK.with_borrow(|hook| {
            get_property(hook, "queue")
                .unchecked_ref::<Array>()
                .push(&action)
        });
        WORK_IN_PROGRESS_ROOT.replace(Some(Rc::new(RefCell::new(Fiber {
            dom: CURRENT_ROOT
                .with_borrow(|current_root| current_root.clone().unwrap().borrow().dom.clone()),
            props: CURRENT_ROOT
                .with_borrow(|current_root| current_root.clone().unwrap().borrow().props.clone()),
            alternate: CURRENT_ROOT.with_borrow(|current_root| current_root.clone()),
            ..Default::default()
        }))));
        NEXT_UNIT_OF_WORK.replace(WORK_IN_PROGRESS_ROOT.with_borrow(|wip_root| wip_root.clone()));
        DELETIONS.replace(Vec::new());
    };
    let closure = Closure::wrap(Box::new(set_state) as Box<dyn Fn(JsValue)>).into_js_value();
    vec![get_property(&hook.clone(), "state"), closure]
}

fn reconciler_children(work_in_progress: FiberMutRef, elements: Vec<JsValue>) {
    let mut index = 0;
    let mut old_fiber = match work_in_progress.clone().borrow().alternate.clone() {
        Some(alternate) => alternate.borrow().child.clone(),
        None => None,
    };
    let mut prev_sibling: Option<FiberMutRef> = None;
    while index < elements.len() || old_fiber.is_some() {
        let element = elements.get(index);
        let mut new_fiber = None;
        let same_type = match (old_fiber.clone(), element) {
            (Some(old_fiber), Some(element)) => {
                old_fiber.borrow().r#type == get_property(element, "type")
            }
            _ => false,
        };
        if same_type {
            let old_fiber_inner = old_fiber.clone().unwrap();
            let old_fiber_inner = old_fiber_inner.borrow();
            let element = element.unwrap();
            new_fiber = Some(Rc::new(RefCell::new(Fiber {
                r#type: old_fiber_inner.r#type.clone(),
                props: get_property(element, "props").unchecked_into(),
                dom: old_fiber_inner.dom.clone(),
                parent: Some(work_in_progress.clone()),
                alternate: old_fiber.clone(),
                effect_tag: EffectTag::Update,
                ..Default::default()
            })))
        }
        if element.is_some() && !same_type {
            let element = element.unwrap();
            new_fiber = Some(Rc::new(RefCell::new(Fiber {
                r#type: get_property(element, "type"),
                props: get_property(element, "props").unchecked_into(),
                parent: Some(work_in_progress.clone()),
                effect_tag: EffectTag::Placement,
                ..Default::default()
            })))
        }
        if old_fiber.is_some() && !same_type {
            let old_fiber = old_fiber.clone().unwrap();
            old_fiber.borrow_mut().effect_tag = EffectTag::Deletion;
            DELETIONS.with_borrow_mut(|arr| arr.push(old_fiber));
        }
        if let Some(old_fiber_inner) = old_fiber.clone() {
            old_fiber = old_fiber_inner.borrow().sibling.clone();
        }
        if index == 0 {
            unsafe { work_in_progress.as_ptr().as_mut().unwrap().child = new_fiber.clone() };
        } else if element.is_some() {
            prev_sibling.clone().unwrap().borrow_mut().sibling = new_fiber.clone();
        }
        prev_sibling = new_fiber;
        index += 1;
    }
}
