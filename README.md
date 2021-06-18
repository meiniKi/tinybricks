# tinybricks
_tinybricks_ is a boot-sector bricks game. The entire code fits into a 512-byte boot sector and can be booted without an underlying operating system. Due to the size constraints, the generated code is highly optimized, and screen memory is used to store variables. Many thanks to [Oscar Toledo](https://github.com/nanochess) and his book that greatly supported me in writing this game.

## Compile and Execute
```nasm -f bin bootbricks.asm -o bootbricks.img```
```qemu-system-x86_64 bootbricks.img```

