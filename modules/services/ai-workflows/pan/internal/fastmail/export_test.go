package fastmail

import "git.sr.ht/~rockorager/go-jmap"

// NewForTest provides a backdoor for black-box testing in fastmail_test.go
func NewForTest(client JMAPClient, id jmap.ID) Service {
	return &fastmail{client: client, id: id}
}
