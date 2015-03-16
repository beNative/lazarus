# Introduction #

The main architecture of Notepas relies heavily on interfaces to achieve loose coupling between the key modules.

All the interfaces are declared in the `ts.Editor.Interfaces` unit. Instances of the key modules supporting those interfaces are created using factories, to reduce dependencies.

In fact to use the editor library in an application you only need to use 2 units:

  * `ts.Editor.Interfaces` with all the interface types
  * `ts.Editor.Factories` to create the required instances of those interface types.