// @ts-check

import { useEffect, useRef, useState } from "react";

export const useStateMachineImpl = (tsApi) => () => {
  const stateRef = useRef(tsApi.initState);
  const [snap, setSnap] = useState({ state: tsApi.initState });

  const tsStateHandle = {
    updateState: (stateFn) => () => {
      stateRef.current = stateFn(stateRef.current)();
      setSnap({ state: stateRef.current });
    },
    readState: () => stateRef.current,
  };

  const dispatch = tsApi.dispatchers(tsStateHandle);
  const state = snap.state.pubState;
  return { state, dispatch };
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
