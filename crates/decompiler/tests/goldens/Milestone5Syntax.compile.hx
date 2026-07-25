private typedef RecoveryAlias = Dynamic;

private enum RecoverySyntaxChoice {
    Ready;
    Payload(value: Int, text: String);
}

private class RecoverySyntaxBase {
    public function new(seed: Int) {}
    public function method(): Int return 1;
}

@:keep
private extern class RecoverySyntaxNatives {
    @:hlNative("std", "itos")
    public static function intToString(value: Int): String;
}

class Milestone5Syntax extends RecoverySyntaxBase {
    var i64: hl.I64;
    var f32: hl.F32;
    var bytes: hl.Bytes;
    var runtimeType: hl.Type;
    var reference: hl.Ref<Int>;
    var nullable: Null<Int>;
    var callback: (Int) -> String;
    var record: { class_: Int };
    var alias: RecoveryAlias;

    public function new(seed: Int) {
        super(seed);
        bytes = new hl.Bytes(4);
        reference = new hl.Ref<Int>(seed);
        nullable = null;
        callback = value -> Std.string(value);
        record = { class_: seed };
        alias = null;
    }

    override public function method(): Int {
        return super.method() + record.class_;
    }

    public dynamic function dynamicCall(value: Int): Int {
        return value * 2;
    }

    static function staticCall(value: Int): Int {
        return value + 1;
    }

    static function lower(): Int return 0;
    static function upper(): Int return 2;

    static function main() {
        var instance = new Milestone5Syntax(2);
        var closure = instance.method;
        var precedence = (1 + 2) * 3 - (4 - 1);
        var values = [instance.method(), staticCall(precedence), closure()];
        values[0] = instance.dynamicCall(values[1]);

        var nativeValues = new hl.NativeArray<Int>(2);
        nativeValues[0] = values.length;
        nativeValues[1] = values[0];

        var sum = 0;
        for (value in values) sum += value;
        for (index in lower()...upper()) sum += index;

        var choice = Payload(sum, 'native=${nativeValues.length}');
        switch (choice) {
            case Ready:
                sum = 0;
            case Payload(value, text):
                trace('${text}: ${Std.string(value)}');
        }

        var class_ = "keyword\\control\n";
        var dynamicValue: Dynamic = { field: class_ };
        Reflect.setField(dynamicValue, "field-name", RecoverySyntaxNatives.intToString(sum));
    }
}
