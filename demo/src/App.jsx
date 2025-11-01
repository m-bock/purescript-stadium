import { useDemoStateMachine } from '../../output/Test.Demo/index.js'

function App() {

  const { state, dispatch } = useDemoStateMachine()

  return (
    <div>
      <h1>Count: {state.count}</h1>
      <button onClick={() => dispatch.countUp()}>Count Up</button>
      <button onClick={() => dispatch.countDown()}>Count Down</button>
    </div>
  )
}

export default App
