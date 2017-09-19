# Grawlix

[![Build badge][]][build status]

> A string of typographical symbols, especially "@#$%&!", used (especially in
> comic strips) to represent an obscenity or swearword.

Grawlix lets you comment on Haskell documentation.

:warning: This is really annoying to use right now! :warning:

Here's how to actually use Grawlix:

1.  Go to <https://grawlix.herokuapp.com/client>.

2.  Select everything and copy it.

3.  Go to some Haskell documentation on Hackage or Stackage. For example,
    <https://hackage.haskell.org/package/salve-0.0.8/docs/Salve.html>.

4.  Open the developer console. On Windows, this is `Ctrl + Shift + I`. (This
    is only tested with Chrome so far.)

5.  Paste all that JavaScript and press `Enter`.

6.  The comment sections should load in. Pretty much nothing will have any
    comments yet.

    ![](https://i.imgur.com/p22Rpdi.png)

7.  Type in a comment and click "Submit". You'll bounce through some redirects,
    including GitHub's OAuth. You should end up back on the page you were on.

8.  Re-open the developer console, re-paste the JavaScript, and re-run it.

9.  Your comment should show up. Hooray!

    ![](https://i.imgur.com/LvU1E6A.png)

[Build badge]: https://travis-ci.org/tfausak/grawlix.svg?branch=master
[build status]: https://travis-ci.org/tfausak/grawlix
