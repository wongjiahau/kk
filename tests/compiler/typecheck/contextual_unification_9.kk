do { 
  let bar 
    :  (o: { f: (n: { x: Integer }) => Integer }) => Integer
    = props => {
      let { f } = props;
      {x: "hi"}.f()
    }
}
