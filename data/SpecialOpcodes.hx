class SpecialOpcodes {
    static function main() {
        var a = 0;
        // https://haxe.org/manual/target-syntax.html#other-platforms
        #if hlbc_special_opcodes
        untyped $prefetch(a, 0);
        untyped $asm(3, 1, a);
        #end
        trace(a);
    }
}
