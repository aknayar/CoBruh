import os
import subprocess
import sys

if __name__ == "__main__":
    TEST_DIR = "./tests/"
    COMPILER_DIR = "../bin/main.exe" if len(sys.argv) == 2 and sys.argv[1] == "ext" else "../_build/default/bin/main.exe"
    LLVM_OUT = "test_tmp.ll"
    num_passed, num_total = 0, 0
    directory = os.fsencode(TEST_DIR)
    i = 0
    for file in os.listdir(directory):
        filename = os.fsdecode(file)
        file_ans_loc = os.path.join(TEST_DIR, filename)
        file_loc = os.path.join(TEST_DIR, '.'.join(filename.split('.')[:-1] + ["bruh"]))

        if filename.endswith(".err") or filename.endswith(".out"):
            i += 1
            result = None
            try:
                result = subprocess.run(f"{COMPILER_DIR} -c {file_loc}".split(), stderr=subprocess.PIPE, stdout=subprocess.PIPE)
            except FileNotFoundError:
                raise Exception('Compiler not found. Try running "dune build" from the CoBruh directory')
            if filename.endswith(".out"):
                with open(LLVM_OUT, 'w') as writer:
                    writer.write(result.stdout.decode("utf-8"))
                result = subprocess.run(f"lli {LLVM_OUT}".split(), stdout=subprocess.PIPE)
            if result.stderr:
                result = result.stderr.decode("utf-8")
            else:
                result = result.stdout.decode("utf-8")
            print(f"Running test case {i} ({filename}):", end=" ")
            num_total += 1
            with open(file_ans_loc, 'r') as f:
                ans = f.read()
                if not ans.strip() == result.strip():
                    print(f"failed: \n\nOutput: {result}\nExpected output: \n{ans}\n")
                else:
                    num_passed += 1
                    print("passed")
    os.remove(LLVM_OUT)
    print(f"\nPassed {num_passed}/{num_total} test cases")

