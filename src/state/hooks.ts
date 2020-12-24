import {useState, useEffect, useRef} from 'react';


type WindowSize = {
  width: number,
  height: number,
}

export function useDelayRender(time: number = 100) {
  const [render, setRender] = useState(false);
  useEffect(() => {
    setTimeout(() => {
      setRender(true);
      console.log("yo");
    }, time);
  }, []);
  return render;
}

export function useWindowSize() {
  const [windowSize, setWindowize] = useState<WindowSize>({
    width: 0,
    height: 0,
  });

  useEffect(() => {
    function resizeHandler() {
      setWindowize({
        width: window.innerWidth,
        height: window.innerHeight,
      });
    }

    window.addEventListener('resize', resizeHandler);
    resizeHandler();

    return () => window.removeEventListener('resize', resizeHandler);
  }, []);
  return windowSize;
}

export function useHover<T extends EventTarget>() {
  const [value, setValue] = useState<boolean>(false);
  const ref = useRef<T>(null);
  const mouseOverHandler = () => setValue(true);
  const mouseOutHandler = () => setValue(false);

  useEffect(() => {
    const node = ref.current;
    if (node) {
      node.addEventListener('mouseover', mouseOverHandler);
      node.addEventListener('mouseout', mouseOutHandler);
    }

    return () => {
      node?.removeEventListener('mouseover', mouseOverHandler);
      node?.removeEventListener('mouseout', mouseOutHandler);
    }

  }, [ref.current]);

  return [ref, value];
}
