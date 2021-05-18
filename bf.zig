const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = *std.mem.Allocator;


const Mult = struct {
    offset: isize,
    mul: u8,
};

const Inst = union (enum) {
    Add: u8,
    Move: isize,
    Input: void,
    Output: void,
    JumpIf: usize,
    JumpNot: usize,
    Offset: ArrayList(*Mult),
};

const Code = ArrayList(Inst);


fn readChar(program: *[*:0]u8) ?u8 {
    const r = program.*[0];
    if (r == 0) return null;
    program.* += 1;
    return r;
}

fn rewind(program: *[*:0]u8) void {
    // what the fuck am I doing?
    program.* -= 1;
}

fn parse_diff(comptime T: type, comptime up: u8, comptime down: u8, program: *[*:0]u8, c: u8) T {
    const zero: T = 0;  // I wish I were dead but this singlehandedly fixes the code
    var n: T = if (c == up) 1 else (zero -% 1);
    while (true) {
        const rc = readChar(program);
        if (rc == null) break;
        const v: T = switch (rc.?) { 
            up => 1, down => zero -% 1,
            else => { rewind(program); break; }
        };
        n +%= v;
    }
    return n;
}

fn parse_(program: *[*:0]u8, alloc: Allocator) Code {
    var code = Code.init(alloc);
    while (true) {
        const rc = readChar(program);
        if (rc == null) break;
        const c = rc.?;
        switch (c) {
            '+', '-' => {
                _ = code.append(Inst{ .Add = parse_diff(u8, '+', '-', program, c) }) catch unreachable;
            },
            '>', '<' => {
                _ = code.append(Inst{ .Move = parse_diff(isize, '>', '<', program, c) }) catch unreachable;
            },
            ',' => { _ = code.append(Inst{ .Input = undefined }) catch unreachable; },
            '.' => { _ = code.append(Inst{ .Output = undefined }) catch unreachable; },
            '[' => {
                const inner = parse_(program, alloc);

                var offsets = ArrayList(*Mult).init(alloc);
                var failed = false;
                var pos: isize = 0;
                var zero_offset: u8 = 0;
                outer: for (inner.items) |i| {
                    switch (i) {
                        Inst.Add => |v| {
                            if (pos == 0) {
                                zero_offset +%= v;
                            } else {
                                for (offsets.items) |offset| {
                                    if (offset.offset == pos) {
                                        offset.mul +%= v;
                                        continue :outer;
                                    }
                                }
                                var m = alloc.create(Mult) catch unreachable;
                                m.* = Mult{ .offset = pos, .mul = v };
                                _ = offsets.append(m) catch unreachable;
                            }
                        },
                        Inst.Move => |v| { pos += v; },
                        else => { failed = true; break; },
                    }
                }

                if (failed or pos != 0 or zero_offset != 255) {
                    _ = code.append(Inst{ .JumpNot = inner.items.len + 1 }) catch unreachable;
                    _ = code.appendSlice(inner.items) catch unreachable;
                    _ = code.append(Inst{ .JumpIf = inner.items.len + 1 }) catch unreachable;
                } else {
                    _ = code.append(Inst{ .Offset = offsets }) catch unreachable;
                }
            },
            ']' => break,
            else => {},
        }
    }
    return code;
}

fn parse(program: []u8, alloc: Allocator) Code {
    return parse_(&@ptrCast([*:0]u8, program), alloc);
}

fn output_code(code: Code, stdout: std.io.Writer(std.fs.File, std.os.WriteError, std.fs.File.write)) void {
    stdout.print("[", .{}) catch unreachable;
    for (code.items) |c| {
        switch (c) {
            Inst.Add => |v| stdout.print("Add({}), ", .{ v }) catch unreachable,
            Inst.Move => |v| stdout.print("Move({}), ", .{ v }) catch unreachable,
            Inst.Input => stdout.print("Input, ", .{}) catch unreachable,
            Inst.Output => stdout.print("Output, ", .{}) catch unreachable,
            Inst.JumpIf => |v| stdout.print("JumpIf({}), ", .{ v }) catch unreachable,
            Inst.JumpNot => |v| stdout.print("JumpNot({}), ", .{ v }) catch unreachable,
            Inst.Offset => |_| stdout.print("Offset(...), ", .{}) catch unreachable,
        }
    }
    stdout.print("]", .{}) catch unreachable;
}


fn au(x: isize) usize { return @intCast(usize, x); }

fn interpret_(
    code: Code,
    ptr: *isize,
    tape: ArrayList(u8),
    stdin: std.io.Reader(std.fs.File, std.os.ReadError, std.fs.File.read),
    stdout: std.io.Writer(std.fs.File, std.os.WriteError, std.fs.File.write),
) ArrayList(u8) {
    var mtape = tape;
    var i: usize = 0;
    while (i < code.items.len) {
        var inst = code.items[i];
        i += 1;
        switch (inst) {
            Inst.Add => |v| {  mtape.items[au(ptr.*)] +%= v; },
            Inst.Move => |v| {
                ptr.* += v;
                if (ptr.* >= mtape.items.len) {
                    var d = au(ptr.*) - (mtape.items.len-1);
                    while (d > 0) {
                        _ = mtape.append(0) catch unreachable;
                        d -= 1;
                    }
                }
                if (ptr.* < 0) {
                    std.debug.print("\npointer moved off left edge of tape\n", .{});
                    std.os.exit(1);
                }
            },
            Inst.Input => mtape.items[au(ptr.*)] = stdin.readByte() catch 0,
            Inst.Output => stdout.writeByte(mtape.items[au(ptr.*)]) catch unreachable,
            Inst.JumpIf => |i_| {
                if (mtape.items[au(ptr.*)] != 0) i -= i_;
            },
            Inst.JumpNot => |i_| {
                if (mtape.items[au(ptr.*)] == 0) i += i_;
            },
            Inst.Offset => |offsets| {
                const v = mtape.items[au(ptr.*)];
                if (v == 0) continue;
                mtape.items[au(ptr.*)] = 0;
                for (offsets.items) |offset| {
                    if (ptr.* + offset.offset >= mtape.items.len) {
                        var d = au(ptr.* + offset.offset) - (mtape.items.len-1);
                        while (d > 0) {
                            _ = mtape.append(0) catch unreachable;
                            d -= 1;
                        }
                    }
                    mtape.items[au(ptr.*+offset.offset)] +%= offset.mul *% v;
                }
            }
        }
    }
    return mtape;
}

fn interpret(alloc: Allocator, code: Code) void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var ptr: isize = 0;
    var tape = ArrayList(u8).init(alloc);
    _ = tape.append(0) catch unreachable;
    _ = interpret_(code, &ptr, tape, stdin, stdout);
}


pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = &arena.allocator;

    var argiter = std.process.args();
    _ = argiter.skip();
    const filename = try argiter.next(alloc).?;
    const code = parse(try std.fs.cwd().readFileAlloc(alloc, filename, 10000000000), alloc);

    interpret(alloc, code);
}
