import pandocfilters as pf

def latex(s):
    return pf.RawBlock('latex', s)

def mk_columns(k, v, f, m):
    if k == "Para":
        value = pf.stringify(v)
        if value.startswith('[') and value.endswith(']'):
            content = value[1:-1]
            if content == "/alert":
                return latex(r'\end{alertblock}')
            elif content.startswith("alert="):
                return latex(r'\begin{alertblock}{%s}' % content[6:])
            if content == "/block":
                return latex(r'\end{block}')
            elif content.startswith("block="):
                return latex(r'\begin{block}{%s}' % content[6:])
            if content == "/example":
                return latex(r'\end{examples}')
            elif content.startswith("example="):
                return latex(r'\begin{examples}{%s}' % content[8:])

if __name__ == "__main__":
    pf.toJSONFilter(mk_columns)

