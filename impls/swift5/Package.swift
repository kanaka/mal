// swift-tools-version:5.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "mal",
    products: [
        // Products define the executables and libraries produced by a package, and make them visible to other packages.
        .executable(name: "step0_repl", targets: ["step0_repl"]),
        .executable(name: "step1_read_print", targets: ["step1_read_print"]),
        .executable(name: "step2_eval", targets: ["step2_eval"]),
        .executable(name: "step3_env", targets: ["step3_env"]),
        .executable(name: "step4_if_fn_do", targets: ["step4_if_fn_do"]),
        .executable(name: "step5_tco", targets: ["step5_tco"]),
        .executable(name: "step6_file", targets: ["step6_file"]),
        .executable(name: "step7_quote", targets: ["step7_quote"]),
        .executable(name: "step8_macros", targets: ["step8_macros"]),
        .executable(name: "step9_try", targets: ["step9_try"]),
        .executable(name: "stepA_mal", targets: ["stepA_mal"])
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(name: "core", dependencies: []),
        .target(name: "step0_repl", dependencies: ["core"]),
        .target(name: "step1_read_print", dependencies: ["core"]),
        .target(name: "step2_eval", dependencies: ["core"]),
        .target(name: "step3_env", dependencies: ["core"]),
        .target(name: "step4_if_fn_do", dependencies: ["core"]),
        .target(name: "step5_tco", dependencies: ["core"]),
        .target(name: "step6_file", dependencies: ["core"]),
        .target(name: "step7_quote", dependencies: ["core"]),
        .target(name: "step8_macros", dependencies: ["core"]),
        .target(name: "step9_try", dependencies: ["core"]),
        .target(name: "stepA_mal", dependencies: ["core"])
    ]
)
