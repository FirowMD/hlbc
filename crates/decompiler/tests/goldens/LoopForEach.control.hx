class LoopForEach {

    static function main() {
        var sum = 0;
        for (i in items()) {
            sum = sum + i;
        }
    }

    static function items(): Array<Int> {
        var __hl_r2 = 12;
        __hl_r2 = 0;
        __hl_r2++;
        __hl_r2++;
        __hl_r2++;
        __hl_r2 = 3;
        var __hl_r0 = [1, 2, 3];
        return __hl_r0;
    }
}
