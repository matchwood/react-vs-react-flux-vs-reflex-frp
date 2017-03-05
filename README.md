# react-vs-react-flux-vs-reflex-frp

Implementations of a simple table in react + redux, react-flux and reflex-frp. All the app does is give you a button which adds 1000 rows to a table when clicked. The purpose of this repo is simply to provide a comparison of performance and memory usage.

## react with redux and immutable
[react-redux](https://github.com/reactjs/react-redux)

    cd react
    npm install
    npm run build
open react/html/table.html in a browser

## react-flux
[react-flux](https://bitbucket.org/wuzzeb/react-flux)

    cd react-flux
    stack build
    make 
open react-flux/html/table.html in a browser

## reflex-frp
[reflex-frp](https://github.com/reflex-frp/reflex)

    cd reflex-frp
    stack build
    make 
open reflex-frp/html/table.html in a browser



