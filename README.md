# Dotfiles

My dotfiles repo.  
Perhaps the only remarkable thing is DotBot bootstrapping. For more detail,
see [anishathalye/dotbot](https://github.com/anishathalye/dotbot), which
provides a lightweight / elegant solution to "bootstrap" dotfiles; i.e.
dotbot downloads itself, then takes care of _symlinking_ dotfiles.  
It's pretty nifty.

## Vim Native Dependencies

A couple of plugins require native dependencies. (VimProc, and YouCompleteMe).

For building YCM with C/C++ support, with system libclang (not recommended):

```
cmake -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
```

Though `ninja` is somewhat quicker (about half the time or so),
if the installation has that.
