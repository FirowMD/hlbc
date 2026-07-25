class MapLiteral {

    static function key(value: Int): String {
        return 'key${value}';
    }

    static function item(value: Int): Int {
        return value * 10;
    }

    static function main() {
        var values = [key(1) => item(1), key(2) => item(2)];
        // HashLink null check: haxe__Log.trace
        var __hl_r7 = values.get("key1");
        var __hl_r8 = (cast {} : Dynamic);
        Reflect.setField(__hl_r8, "fileName", "data/MapLiteral.hx");
        Reflect.setField(__hl_r8, "lineNumber", 15);
        Reflect.setField(__hl_r8, "className", "MapLiteral");
        Reflect.setField(__hl_r8, "methodName", "main");
        haxe.Log.trace(__hl_r7, __hl_r8);
    }
}
