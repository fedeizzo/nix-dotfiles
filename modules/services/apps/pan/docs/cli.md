# Pan CLI

The supported command-line workflows use Lunch Money only; they do not connect to Fastmail,
Matrix, or the assistant.

```sh
pan --config pan.yaml review-transaction
pan --config pan.yaml review-transaction --json
pan --config pan.yaml apply-transaction 42 --confirm
```

`apply-transaction` refuses to mutate unless `--confirm` is present and applies only the supplied
transaction ID. Exit status is 0 on success, 2 for configuration errors, 3 when no review is
available, and 4 for provider failures. Other workflow errors use status 1.
