
assert () {
    if ! eval "${2}"; then
        echo "assert failure line ${1}"
        exit 1
    fi
}

assert_eq () {
    if eval "${3}"; then
        if [[ "${2}" != "${r}" ]]; then
            echo "assert_eq failure line ${1}: '${2}' != '${r}'"
            exit 1
        fi
    else 
        echo "assert_eq failure line ${1}: could not evaluate '${3}'"
        exit 1
    fi
}

TEST_RE () {
    r=
    READ_STR "${1}"
    EVAL "${r}" ${REPL_ENV}
}
