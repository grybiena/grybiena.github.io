[halogen-infinite-scroll](https://github.com/grybiena/halogen-infinite-scroll) is a Halogen component for building scrollable feeds.

One of the goals of this library is to facilitate loading and unloading of content from both the top and bottom of the feed.

The default options are configured to only load content at the bottom of the feed. This works well and seems to be the standard design that everyone implements. Typically loading at the top is done with a "refresh" button at the top of the feed.

Seemlessly loading content at the top of the feed would be a nice feature. Unfortunately it seems that it is tricky to implement - loading and unloading at the top of the feed causes issues on some platforms that severely impact the UX. The reason for this is that loading or unloading content at the top requires modifying the scroll offset to account for the content added or removed. In some browsers the existing implementation works flawlessly and in others it causes very noticable and sometimes pathological jumpiness of the feed since the scroll offset also gets changed by unknown forces within the browser implementation. The precise cause of this (and a potential solution) is yet to be fully determined but seems to be dependent on browser implementation of features such as smooth scrolling. [Smooth scrolling is a feature designed to allow content that is out of view above the viewport to load without affecting the scroll position of visible content.]

The below example includes a test harness to show the state of the feed and allow toggling of some test features.

In the future I plan to publish a more detailed write up on the design and perhaps dive into an exploration of the root causes of the issues described above. In the mean time if you are looking to implement a feed in purescript then this library does a decent job of doing that in the traditional way where content is only loaded at the bottom.

#### Example
The feed is on the left. The terminal to the right logs information about the feed state. Below the terminal are some controls for testing the feed.

If you are interested in bidirectional feed loading or feel like contributing feel free to get in touch. Reporting your experience of using the example below with bidirectional loading enabled, including browser/OS version, and a description of behaviour would be greatly appreciated. 

<iframe width=900px height=900px src="/examples/halogen-infinite-scroll/index.html">
