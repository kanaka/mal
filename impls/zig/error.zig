const assert = @import("std").debug.assert;
const MalType = @import("types.zig").MalType;

pub const MalError = error {
    SystemError,
    ApplyError,
    KeyError,
    ThrownError,
    TypeError,
    ArgError,
    Overflow,
    DivisionByZero,
    OutOfBounds,

    OutOfMemory,

    InvalidCharacter,

    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    DeviceBusy,
    InvalidArgument,
    AccessDenied,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    WouldBlock,
    ConnectionResetByPeer,
    Unexpected,

    InvalidUtf8,
    SharingViolation,
    PathAlreadyExists,
    FileNotFound,
    PipeBusy,
    NameTooLong,
    InvalidWtf8,
    BadPathName,
    NetworkNotFound,
    AntivirusInterference,
    SymLinkLoop,
    ProcessFdQuotaExceeded,
    SystemFdQuotaExceeded,
    NoDevice,
    IsDir,
    NotDir,
    FileLocksNotSupported,
    FileBusy,
    Unseekable,
    ConnectionTimedOut,
    NotOpenForReading,
    SocketNotConnected,
};

var error_data: ?*MalType = null;

pub fn throw(mal: *MalType) MalError {
    assert(error_data == null);
    error_data = mal;
    mal.incref();
    return MalError.ThrownError;
}

pub fn get_error_data() ?*MalType {
    defer error_data = null;
    return error_data;
}
