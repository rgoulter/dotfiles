# Dotfiles

My dotfiles repo.  

Makes use of [anishathalye/dotbot](https://github.com/anishathalye/dotbot)
for managing symlinks.

## Vim Native Dependencies

A couple of plugins require native dependencies. (VimProc, and YouCompleteMe).

For building YCM with C/C++ support, with system libclang (not recommended by YCM):

```
cmake -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
```
