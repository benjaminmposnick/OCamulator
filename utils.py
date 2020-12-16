import functools
import operator


def to_ocaml(X):
    """
    Converts matrix from string format outputted by Matlab to OCaml syntax

    Note: the Matlab function to convert matrix to string to use as input for
    this function is:
        regexprep( mat2str(a), {'\[', '\]', '\s+'}, {'', '', ','})
    """
    contents = X[1:len(X)-1]
    rows = contents.split(";")
    entries = map((lambda r: r.split(",")), rows)
    rows = map((lambda e: map((lambda x: str(round(float(x), 4))), e)), entries)
    rows = map((lambda e: ";".join(e)), rows)
    contents = "];[".join(rows)
    contents = contents.replace("-", "~-.")
    ocaml_str = "[[" + contents + "]]"
    return ocaml_str
