import React from 'react'
import ReactDOM from 'react-dom'
import {Provider, connect} from 'react-redux'
import * as redux from 'redux'



import Immutable from 'immutable'

const Entry = Immutable.Record({a: '', b: '', c: 0, d: 0})

const ADD_ENTRIES = 'ADD_ENTRIES'
const rootReducer = function (st, action) {
  if (action.type === ADD_ENTRIES) {
    var list = st.get('entries')
    while (list.size < 1000) {
      list = list.push(new Entry({a: 'Some', b: 'Text here', c: 1.23, d: 424242}))
    }
    return st.set('entries', list)
  }

  return st
}
const state = Immutable.Map({entries: Immutable.List()})

const store = redux.createStore(rootReducer, state)


const App = connect(st => { return {st: st} })(props => {

  const {st, dispatch} = props

  const dispatchAction = () => {
    dispatch({type: ADD_ENTRIES})
  }
  return <div>
  <h3>React with Redux</h3>
  <button onClick={dispatchAction}>Add rows</button>

  <table>
  <thead>
  <tr><th>a</th><th>b</th><th>c</th><th>d</th></tr>
  </thead>
  <tbody>
  {st.get('entries').map((e, k) => {
    return <tr key={k}><td>{e.get('a')}</td><td>{e.get('b')}</td><td>{e.get('c')}</td><td>{e.get('d')}</td></tr>
  })


  }

  </tbody>
  </table>


  </div>


})



ReactDOM.render(
  <Provider store={store}>
  <App />
  </Provider>, document.getElementById('table-example'))
