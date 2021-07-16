# split-text-regex
`split-text` is a text manipulation tool to
split lines of texts from input stream into multiple files by regex.

`split-text` takes its input from `stdin` stream and
write the output to one or more files, specified by its arguments.

# Example:

Suppose there's `myapp.log` file below.

```
[info] 2021-01-03 01:00 backup to '/opt/myapp/bkup01'
[error] 2021-01-03 01:00 backup process failed (-1)
[info] 2021-01-03 01:02 send notification to admin@example.com
[info] 2021-02-03 01:00 backup to '/opt/myapp/bkup02'
[info] 2021-03-03 01:00 backup to '/opt/myapp/bkup03'
```

You can use the following command to split the lines of this file to
`myapp.error` and `myapp.nonerror`.

```shell
cat myapp.log | split-text\
  '^\[error\]\s+(.*)$' '\1' 'myapp.error'\
  '.*' '\0' 'myapp.nonerror'
```

Running this command creates `myapp.error` file

```
2021-01-03 01:00 backup process failed (-1)
```

and `myapp.nonerror` file below.

```
[info] 2021-01-03 01:00 backup to '/opt/myapp/bkup01'
[info] 2021-01-03 01:02 send notification to admin@example.com
[info] 2021-02-03 01:00 backup to '/opt/myapp/bkup02'
[info] 2021-03-03 01:00 backup to '/opt/myapp/bkup03'
```
