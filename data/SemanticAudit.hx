class SemanticAudit {
    var bias:Int;

    public function new(bias:Int) {
        this.bias = bias;
    }

    public function boundUnnamed(value:Int):Int {
        return callMapper((item) -> applyBias(item), value);
    }

    function applyBias(value:Int):Int {
        return value + bias;
    }

    static function callMapper(mapper:Int -> Int, value:Int):Int {
        return mapper(value);
    }

    public static function capturedEnvironment(value:Int, previous:Int):() -> Int {
        return () -> {
            previous = value + previous;
            return previous;
        };
    }

    public static function buildAndCopy(count:Int):Array<Int> {
        var values = new Array<Int>();
        for (index in 0...count) {
            values.push(index);
        }
        return values.copy();
    }

    static function main() {
        var audit = new SemanticAudit(2);
        audit.boundUnnamed(1);
        capturedEnvironment(1, 2)();
        buildAndCopy(2);
    }
}
