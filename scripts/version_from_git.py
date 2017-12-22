#!/usr/bin/env python
import parse
import sys

git_head = parse.parse('ref: {}', sys.argv[1])[0].strip()
tag_description = sys.argv[2].strip()

head_parts = git_head.split('/')
if len(head_parts) > 2:
    if head_parts[-2] in ['release', 'hotfix', 'support']:
        print head_parts[-1]
    elif len(head_parts) == 3 and head_parts[-1] in ['master']:
        tag_description_parts = tag_description.split('-')
        (major, minor, fix) = parse.parse('{:d}.{:d}.{:d}', tag_description_parts[0])
        print '%d.%d.%d' % (major, minor, fix)
    else:
        print tag_description
else:
    print tag_description
