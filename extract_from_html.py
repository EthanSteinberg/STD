import lxml.html
from lxml.cssselect import CSSSelector

final = {}

all_laws = set()

with open('data/State HIV Laws _ Law _ Policies _ HIV_AIDS _ CDC.html') as f:
    tree = lxml.html.parse(f)

    sel = CSSSelector('a.onThisPageAnchor.tp-link-policy')

    # Apply the selector to the DOM tree.
    results = sel(tree)

    for result in results:
        row_selector = CSSSelector('td:first-child')

        laws = set()

        for a in row_selector(result.getparent()):
            if a.text is not None and a.text.strip() != '':
                laws.add(a.text.strip())

        all_laws |= laws

        final[result.get('title')] = laws

all_laws = list(all_laws)

with open('data/policies1.csv', 'w') as f:
    f.write('State\t' + '\t'.join(all_laws) + '\n')

    for state, state_laws in final.items():
        f.write(state + '\t')

        flags = ['1' if law in state_laws else '0' for law in all_laws]
        f.write('\t'.join(flags) + '\n')


