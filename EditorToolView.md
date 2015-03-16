The editor manager maintains a list of registered toolviews which extend the editor view(s) with operations that can be performed to one or more editor views that are open.

Each toolview implements the `IEditorToolView` interface. Once registered it can interact with the key modules provided by the editor manager.