// @ts-check

import { useEffect, useRef, useState } from "react";

export const useStateMachineImpl = (tsApi) => () => {
  const stateRef = useRef(tsApi.initState);
  const [snap, setSnap] = useState({ state: tsApi.initState });

  // The handle closes over the ref and the setter, which React keeps for
  // the component's lifetime, so one handle serves every render.
  const handleRef = useRef(/** @type {any} */ (null));
  if (handleRef.current === null) {
    handleRef.current = {
      updateState: (stateFn) => () => {
        stateRef.current = stateFn(stateRef.current)();
        setSnap({ state: stateRef.current });
      },
      readState: () => stateRef.current,
    };
  }

  // The dispatchers as this render's config builds them...
  const latestRef = useRef(/** @type {any} */ (null));
  latestRef.current = tsApi.dispatchers(handleRef.current);

  // ...behind one object made on the first render, whose functions call
  // the latest ones. `dispatch` keeps its identity from render to render,
  // as `useReducer`'s does, so `useEffect(f, [dispatch])` runs `f` once.
  const dispatchRef = useRef(/** @type {any} */ (null));
  if (dispatchRef.current === null) {
    dispatchRef.current = followLatest(latestRef.current, () => latestRef.current);
  }

  const state = snap.state.pubState;
  return { state, dispatch: dispatchRef.current };
};

// A stand-in for `shape` that reads `readLatest()` on every use: a function
// calls the latest function at the same place, a record is followed field by
// field, and any other value is read when it is read.
const followLatest = (shape, readLatest) => {
  if (typeof shape === "function") {
    return (...args) => readLatest()(...args);
  }
  if (shape !== null && typeof shape === "object" && !Array.isArray(shape)) {
    const out = {};
    for (const key of Object.keys(shape)) {
      const field = shape[key];
      const readField = () => readLatest()[key];
      if (typeof field === "function" || (field !== null && typeof field === "object" && !Array.isArray(field))) {
        out[key] = followLatest(field, readField);
      } else {
        Object.defineProperty(out, key, { enumerable: true, get: readField });
      }
    }
    return out;
  }
  return shape;
};

/** @type {<A>(cb: () => () => void, eq: (a: A) => (b: A) => boolean, dep: A) => void} */
export const useEffectEq = (cb, eq, dep) => {
  const prevCount = useRef(0);
  const prevDep = useRef(dep);

  prevDep.current = dep;
  prevCount.current = eq(prevDep.current)(dep)
    ? prevCount.current
    : prevCount.current + 1;

  return useEffect(() => {
    return cb();
  }, [prevCount.current]);
};
