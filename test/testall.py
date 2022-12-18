import os
import subprocess

TEST_DIR = "./tests/"
COMPILER_DIR = "../_build/default/bin/main.exe"
num_passed, num_total = 0, 0
directory = os.fsencode(TEST_DIR)
    
for i, file in enumerate(os.listdir(directory)):
    filename = os.fsdecode(file)
    file_ans_loc = os.path.join(TEST_DIR, filename)
    file_loc = os.path.join(TEST_DIR, '.'.join(filename.split('.')[:-1] + ["bruh"]))

    if filename.endswith(".err") or filename.endswith(".out"):
        try:
            result = subprocess.run(f"{COMPILER_DIR} -c {file_loc}".split(), stdout=subprocess.PIPE)
        except FileNotFoundError:
            raise Exception("Compiler not found. Try running "dune build" from the CoBruh directory")
        if filename.endswith(".out"):
            with open("main.ll", 'w') as writer:
                writer.write(result.stdout.decode("utf-8"))
            result = subprocess.run("lli main.ll".split(), stdout=subprocess.PIPE)
        result = result.stdout.decode("utf-8") 
        print(f"Running test case {(i + 1) // 2} ({filename}):", end=" ")
        num_total += 1
        with open(file_ans_loc, 'r') as f:
            ans = f.read()
            if not ans.strip() == result.strip():
                print(f"failed: \n\nOutput: {result}\nExpected output: \n{ans}\n")
            else:
                num_passed += 1
                print("passed")
print(f"\nPassed {num_passed}/{num_total} test cases")