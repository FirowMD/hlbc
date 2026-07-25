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
        var instance: HaxeRecovery = null;
        var direct: Int = 0;
        var virtualClosure: () -> Int = null;
        var dynamicCall: Int = 0;
        var closureCall: Int = 0;
        var length: Int = 0;
        var nativeLength: Int = 0;
        var nativeValues: hl.NativeArray<Dynamic> = null;
        var sum: Int = 0;
        var __hl_r13: Int = 0;
        var __hl_r14: Array<Int> = null;
        var item: Int = 0;
        var index: Int = 0;
        var global: RecoveryChoice = null;
        var payload: RecoveryChoice = null;
        var text: String = null;
        var __hl_r27: Dynamic = null;
        var __hl_r15: Int = 0;
        var sum__hl_13: Int = 0;
        var number: Int = 0;
        var __hl_state = 0;
        var __hl_running = true;
        while (__hl_running) {
            switch (__hl_state) {
                case 0:
                    instance = new HaxeRecovery(4);
                    direct = instance.method();
                    // closure : method@194
                    virtualClosure = instance.method;
                    // HashLink null check: instance.callback
                    dynamicCall = instance.callback(direct);
                    // HashLink null check: virtualClosure
                    closureCall = virtualClosure();
                    length = 3;
                    nativeValues = new hl.NativeArray<Int>(2);
                    nativeValues[0] = length;
                    nativeValues[1] = direct + 1;
                    nativeLength = nativeValues.length;
                    sum = 0;
                    __hl_r13 = 0;
                    __hl_r14 = items();
                    __hl_state = 1;
                case 1:
                    // HashLink null check: __hl_r14
                    if (__hl_r14.length <= __hl_r13) {
                        __hl_state = 6;
                    } else {
                        __hl_state = 2;
                    }
                case 2:
                    if (__hl_r14.length > __hl_r13) {
                        __hl_state = 4;
                    } else {
                        __hl_state = 3;
                    }
                case 3:
                    item = 0;
                    __hl_state = 5;
                case 4:
                    item = __hl_r14[__hl_r13];
                    __hl_state = 5;
                case 5:
                    __hl_r13++;
                    sum = sum + item;
                    __hl_state = 1;
                case 6:
                    __hl_r13 = lower();
                    __hl_r15 = upper();
                    __hl_state = 7;
                case 7:
                    if (__hl_r15 <= __hl_r13) {
                        __hl_state = 9;
                    } else {
                        __hl_state = 8;
                    }
                case 8:
                    index = __hl_r13;
                    __hl_r13++;
                    sum = sum + index;
                    __hl_state = 7;
                case 9:
                    global = Ready;
                    payload = Payload(sum, 'native=${nativeLength}');
                    // HashLink null check: payload
                    __hl_r13 = Type.enumIndex(payload);
                    switch (__hl_r13) {
                        case 0: __hl_state = 11;
                        case 1: __hl_state = 12;
                        default: __hl_state = 13;
                    }
                case 10:
                    __hl_state = 13;
                case 11:
                    sum__hl_13 = 0;
                    __hl_state = 13;
                case 12:
                    number = (cast Type.enumParameters(payload)[0] : Int);
                    text = (cast Type.enumParameters(payload)[1] : String);
                    // HashLink null check: haxe__Log.trace
                    __hl_r27 = (cast {} : Dynamic);
                    Reflect.setField(__hl_r27, "fileName", "data/HaxeRecovery.hx");
                    Reflect.setField(__hl_r27, "lineNumber", 59);
                    Reflect.setField(__hl_r27, "className", "HaxeRecovery");
                    Reflect.setField(__hl_r27, "methodName", "main");
                    haxe.Log.trace('${text}, sum=${number}', __hl_r27);
                    __hl_state = 13;
                case 13:
                    return;
                default:
                    __hl_running = false;
            }
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
