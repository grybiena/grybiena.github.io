Purescript has a powerful and easy to use foreign function interface (FFI). This lets you make bindings into javascript code so you can compile anything javascript directly into your project.

#### Here's an example

This blog is about software engineering so it contains many snippets of code which _must_ be highlighted appropriately. This is easy to do with the wonderful [highlight.js](https://github.com/highlightjs/highlight.js) library.

Since there are no existing purescript libraries published on [pursuit](https://pursuit.purescript.org) that bind to highlightjs that means we have to do the work ourselves.

Fortunately creating a binding to javascript from purescript is trivial. To bring this library into a purescript project all we have to do is write an FFI binding, `npm install highlight.js`, and that's it.

Creating a binding is as simple as declaring a purescript type as a _foreign import_ and writing a corresponding javascript module that exposes a function of this type.

```haskell
module Code.Highlight where
import Prelude
import Effect (Effect)

foreign import highlightAll :: Effect Unit
```

Now the purescript compiler will look for a function named _highlightAll_ in the corresponding _src/Code/Highlight.js_ file and assume that it has the type specified.

```javascript
const hljs = require('highlight.js');

export const highlightAll = () => {
  hljs.highlightAll();
};
```

If you specify the wrong type this may lead to errors but thankfully the type of this function is simply _Effect Unit_ which means that it runs some sort of effectful code and returns no information to the caller. Now we can call this function from purescript to highlight all of the code blocks that appear in our document. Easy.
