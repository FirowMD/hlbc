class MapLiteral {
    static function key(value:Int):String {
        return 'key$value';
    }

    static function item(value:Int):Int {
        return value * 10;
    }

    static function main() {
        var values = [
            key(1) => item(1),
            key(2) => item(2),
        ];
        trace(values.get("key1"));
    }
}
