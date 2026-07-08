# Things to fix

HTML uses get-entity-id, this should be the function name for all namespaces.

Reconsider the JSON, Lacinia layers, Lacinia should rely on the JSON parsing to
do some of the keyword coercion if it needs to.  Or do we keep it separate.

Add a CSV module, it could handle nested values with a dot notation, ie
manager.given-name and managers`[0]`.given-name for the naming.
