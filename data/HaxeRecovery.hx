class HaxeRecovery extends RecoveryBase {
    public var value:Int;

    public function new(seed:Int) {
        super(seed);
        value = seed + 1;
    }

    override public function method():Int {
        return super.method() + value;
    }

    public dynamic function callback(value:Int):Int {
        return value * 2;
    }

    static function items():Array<Int> {
        return [1, 2, 3];
    }

    static function lower():Int {
        return 0;
    }

    static function upper():Int {
        return 3;
    }

    static function main() {
        var instance = new HaxeRecovery(4);
        var direct = instance.method();
        var virtualClosure = instance.method;
        var dynamicCall = instance.callback(direct);
        var closureCall = virtualClosure();

        var values = [direct, dynamicCall, closureCall];
        values[1] = values[0] + 1;
        var length = values.length;

        var nativeValues = new hl.NativeArray<Int>(2);
        nativeValues[0] = length;
        nativeValues[1] = values[1];
        var nativeLength = nativeValues.length;

        var sum = 0;
        for (item in items()) {
            sum += item;
        }
        for (index in lower()...upper()) {
            sum += index;
        }

        var global = Ready;
        var payload = Payload(sum, 'native=$nativeLength');
        switch (payload) {
            case Ready:
                sum = 0;
            case Payload(number, text):
                trace('$text, sum=$number');
        }
    }
}

class RecoveryBase {
    var seed:Int;

    public function new(seed:Int) {
        this.seed = seed;
    }

    public function method():Int {
        return seed;
    }
}

enum RecoveryChoice {
    Ready;
    Payload(number:Int, text:String);
}
