import { createElement, render, start_work_loop, useState } from "../../pkg/react";

start_work_loop();

function HelloWorld() {
    const [state, setState] = useState(0);
    console.log(state);

    return createElement("h1", { title: "Hello", onClick: () => setState((c) => c + 1) }, [
        `Hello world ${state}`,
    ]);
}

const root = document.getElementById("root")!;

render({ type: HelloWorld }, root);
