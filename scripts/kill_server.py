import os
import signal
import sys
from subprocess import check_output, run, PIPE, DEVNULL, CompletedProcess


def runcheck(cmd):
    return check_output(
        cmd,
        shell=True,
        stderr=DEVNULL,
        text=True).strip()


def netstat() -> CompletedProcess:
    return run(["netstat", "-lnp"],
               stdout=PIPE,
               stderr=DEVNULL,
               text=True)


def grep(port: int, stdin) -> CompletedProcess:
    return run(["grep", ":" + str(port)],
               stdout=PIPE, input=stdin.stdout, text=True)


def awk(stdin) -> CompletedProcess:
    return run(["awk", "{print $7}"],
               stdout=PIPE, input=stdin.stdout, text=True)


def cut(de: str, n: int, stdin) -> CompletedProcess:
    return run(["cut", f"-d{de}", f"-f{n}"],
               stdout=PIPE, input=stdin.stdout, text=True)


def find_server_pid(port: int) -> int:
    server_pid: int = -1

    p1 = netstat()
    p2 = grep(port, p1)
    cmd = awk(p2)
    cmdpid = cut('/', 1, cmd)
    cmdpname = cut('/', 2, cmd)

    try:
        if cmdpname.stdout.strip() != "node":
            raise RuntimeError("port 3000 is not used by a node process")

        pid = cmdpid.stdout.strip()
        server_pid = int(pid)
        return int(server_pid)
    except ValueError:
        if (len(pid) == 0):
            print("The server might not be online", file=sys.stderr)
        else:
            print(f"this is not a pid: {pid}\n", file=sys.stderr)
        sys.exit(-1)
    except RuntimeError as e:
        print("netstat result: ", cmd.stdout.strip())
        print(e)
        sys.exit(-1)


def kill(pid: int) -> bool:
    flag = True
    try:
        os.kill(pid, signal.SIGTERM)
    except Exception as e:
        print(f"Failed to send SIGTERM due to {e} try SIGKILL",
              file=sys.stderr)
        flag = False
    return flag


if __name__ == "__main__":
    port = 3000
    if len(sys.argv) > 1:
        port = int(sys.argv[1])

    pid = find_server_pid(port)
    if kill(pid):
        print("failed to kill, try fuser...")
        run(["fuser", "-k", f"{port}/tcp"], stdout=DEVNULL, stderr=DEVNULL)
        if grep(port, netstat()).stdout != "":
            print(f"failed to stop the server on port :{port}")
