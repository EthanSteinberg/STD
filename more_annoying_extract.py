import requests
import lxml.html
from lxml.cssselect import CSSSelector

def get_states():
    root_url = 'http://www.hivlawandpolicy.org/state-hiv-laws'

    root_page = requests.get(root_url).text

    tree = lxml.html.fromstring(root_page)

    sel = CSSSelector('.view-state-listing a')

    elems = sel(tree)

    print(len(elems))

    return [elem.get('href').split('/')[2] for elem in elems]

result = {}

for state in get_states():
    while True:
        try:
            print('try', state)
            page = requests.get('http://www.hivlawandpolicy.org/states/' + state).text
            break
        except:
            continue

    tree = lxml.html.fromstring(page)

    field_set_sel = CSSSelector('.field-group-fieldset')

    field_sets = field_set_sel(tree)

    temp = {}

    for field_set in field_sets:
        legend_sel = CSSSelector('.fieldset-legend')

        legend_elems = legend_sel(field_set)

        assert len(legend_elems) == 1

        legend = legend_elems[0].text

        field_sel = CSSSelector('.field.field-type-list-text')

        elems = field_sel(field_set)

        for elem in elems:
            label_sel = CSSSelector('.field-label')

            label_elem = label_sel(elem)[0]

            label = label_elem.text

            value_sel = CSSSelector('.field-item')
            value_elems = value_sel(elem)

            assert len(value_elems) == 1

            value_elem = value_elems[0]

            values = list(value_elem.itertext())

            assert len(values) == 1

            value = values[0]

            assert value in ('Yes', 'No')

            integer = '1' if value == 'Yes' else '0'

            temp[legend + '|' + label] = integer

    result[state.replace('-', ' ').title()] = temp

all_values = set()
for state, values in result.items():
    all_values |= set(values)

all_values = list(all_values)

all_values.sort()

with open('data/policies2.csv', 'w') as f:
    f.write('State\t' + '\t'.join(all_values) + '\n')

    for state, state_laws in result.items():
        f.write(state + '\t')

        flags = [state_laws.get(law, '-1') for law in all_values]
        f.write('\t'.join(flags) + '\n')