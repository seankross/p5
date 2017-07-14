HTMLWidgets.widget({

  name: 'p5',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var sketch = document.createElement('script');
        sketch.type = 'text/javascript';
        sketch.appendChild(document.createTextNode(x.setup));
        sketch.appendChild(document.createTextNode(x.draw));
        document.head.appendChild(sketch);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
