class LoopRange {
    static function main() {
        var sum = 0;
        for (i in lower()...upper()) {
            sum += i;
        }
    }

    static function lower():Int {
        return 1;
    }

    static function upper():Int {
        return 4;
    }
}
