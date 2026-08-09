class HaxeRecovery extends RecoveryBase {
    var value: Int;

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

    static function lower(): Int {
        return 0;
    }

    static function upper(): Int {
        return 3;
    }

    static function main() {
        var instance = new HaxeRecovery(4);
        var direct = instance.method();
        // closure : method@194
        var virtualClosure = instance.method;
        // HashLink null check: instance.callback
        var dynamicCall = instance.callback(direct);
        // HashLink null check: virtualClosure
        var closureCall = virtualClosure();
        var length = 3;
        var nativeValues = new hl.NativeArray<Int>(2);
        nativeValues[0] = length;
        nativeValues[1] = direct + 1;
        var nativeLength = nativeValues.length;
        var sum = 0;
        for (item in items()) {
            sum = sum + item;
        }
        for (index in lower()...upper()) {
            sum = sum + index;
        }
        var global = Ready;
        var payload = Payload(sum, 'native=${nativeLength}');
        // HashLink null check: payload
        __hl_r13 = Type.enumIndex(payload);
        switch (__hl_r13) {
            case 0:
                var sum = 0;
            case 1:
                var number = (cast Type.enumParameters(payload)[0] : Int);
                var text = (cast Type.enumParameters(payload)[1] : String);
                // HashLink null check: haxe__Log.trace
                var __hl_r27 = (cast {} : Dynamic);
                Reflect.setField(__hl_r27, "fileName", "data/HaxeRecovery.hx");
                Reflect.setField(__hl_r27, "lineNumber", 59);
                Reflect.setField(__hl_r27, "className", "HaxeRecovery");
                Reflect.setField(__hl_r27, "methodName", "main");
                haxe.Log.trace('${text}, sum=${number}', __hl_r27);
        }
    }

    override function method(): Int {
        var __hl_r1 = super.method();
        return __hl_r1 + this.value;
    }

    dynamic function callback(value: Int): Int {
        return value * 2;
    }

    function new(seed: Int) {
        super(seed);
        if (this.callback != null) {} else {
            // closure : callback@195
            this.callback = this.callback;
        }
        this.value = seed + 1;
    }
}

enum RecoveryChoice {
    Ready;
    Payload(arg0: Int, arg1: String);
}

class RecoveryBase {
    public var seed: Int;
    public function new(arg0: Int) {}
    public function method(): Int return cast null;
}
