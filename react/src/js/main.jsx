import React from 'react'
import jq from 'jquery'
import ReactDOM from 'react-dom'
import {Provider, connect} from 'react-redux'
import * as redux from 'redux'



import Immutable from 'immutable'

import * as _ from 'lodash'

const ADD_ENTRIES = 'ADD_ENTRIES'
const rootReducer = function(st, action){



  if (action.type === ADD_ENTRIES){
      var list = st.get('entries')
      list = list.push({a: 'something'})
      return st.set('entries', list)
  }

  return st
}
const state = Immutable.Map({entries: Immutable.List()})

const store = redux.createStore(rootReducer, state)


const App = connect(st => {return {st: st}}) (props => {

  const {st, dispatch} = props

  const dispatchAction = () => {
    dispatch({type: ADD_ENTRIES})
  }
  return <div>
  <button onClick={dispatchAction}>Add rows</button>

  <table>
  <thead>
  <tr><th>a</th><th>b</th><th>c</th><th>d</th></tr>
  </thead>
  <tbody>
  {st.get('entries').map((e) => {
    return <tr><td>{e.a}</td></tr>
  })


  }

  </tbody>
  </table>


  </div>


})



function run () {
      ReactDOM.render(
      <Provider store={store}>
      <App  />
      </Provider>,  document.getElementById('table-example'))



}

jq(document).ready(function () {
  run()
})

11