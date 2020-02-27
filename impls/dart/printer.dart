import 'types.dart';

String pr_str(MalType data, {bool print_readably: true}) {
  if (data is MalSymbol) {
    return data.value;
  } else if (data is MalInt) {
    return '${data.value}';
  } else if (data is MalList) {
    var printedElements =
        data.elements.map((e) => pr_str(e, print_readably: print_readably));
    return '(${printedElements.join(" ")})';
  } else if (data is MalVector) {
    var printedElements =
        data.elements.map((e) => pr_str(e, print_readably: print_readably));
    return '[${printedElements.join(" ")}]';
  } else if (data is MalHashMap) {
    var printedElements = <String>[];
    data.value.forEach((key, value) {
      printedElements.add(pr_str(key, print_readably: print_readably));
      printedElements.add(pr_str(value, print_readably: print_readably));
    });
    return '{${printedElements.join(" ")}}';
  } else if (data is MalString) {
    if (print_readably) {
      var readableValue = data.value
          .replaceAll('\\', r'\\')
          .replaceAll('\n', r'\n')
          .replaceAll('\"', r'\"');
      return '"$readableValue"';
    } else {
      return '${data.value}';
    }
  } else if (data is MalKeyword) {
    return ':${data.value}';
  } else if (data is MalBool) {
    return '${data.value}';
  } else if (data is MalNil) {
    return 'nil';
  } else if (data is MalBuiltin) {
    return '#<built in function>';
  } else if (data is MalClosure) {
    return '#<function>';
  } else if (data is MalAtom) {
    return "(atom ${pr_str(data.value, print_readably: print_readably)})";
  }
  throw new ArgumentError("Unrecognized type: ${data.runtimeType}");
}
