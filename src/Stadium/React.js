// @ts-check

import { useEffect, useRef, useState } from "react";

export const useStateMachine = (tsApi) => () => {
  const [ref, setSt] = useState({ state: tsApi.initState });

  const tsStateHandle = {
    updateState: (stateFn) => () =>
      setSt((ref) => {
        ref.state = stateFn(ref.state)();
        return { ...ref };
      }),
    readState: () => {
      return ref.state;
    },
  };

  const dispatch = tsApi.dispatchers(tsStateHandle);
  const state = ref.state.pubState;

  return { state, dispatch };
};

/** @type {<A>(cb: () => () => void, eq: (a: A, b: A) => boolean, dep: A) => void} */
export const useEffectEq = (cb, eq, dep) => {
  const prevCount = useRef(0);
  const prevDep = useRef(dep);

  prevDep.current = dep;
  prevCount.current = eq(prevDep.current, dep)
    ? prevCount.current
    : prevCount.current + 1;

  return useEffect(() => {
    return cb();
  }, [prevCount.current]);
};
