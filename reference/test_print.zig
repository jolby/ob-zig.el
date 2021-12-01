const std = @import("std");

pub fn main() !void {
    std.debug.warn("{}\n", .{42});
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{42});
}
