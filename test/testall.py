import os
import subprocess

TEST_DIR = "./tests/"
ctr = 0
directory = os.fsencode(TEST_DIR)
    
for file in os.listdir(directory):
    ctr += 1
    filename = os.fsdecode(file)
    file_ans_loc = os.path.join(TEST_DIR, filename)
    file_loc = os.path.join(TEST_DIR, '.'.join(filename.split('.')[:-1] + ["bruh"]))
    if filename.endswith(".err") or filename.endswith(".out"):
        result = subprocess.run(f"dune exec -- CoBruh -c {file_loc}".split(), stdout=subprocess.PIPE)
        if filename.endswith(".out"):
            with open("main.ll", 'w') as writer:
                writer.write(result.stdout.decode("utf-8"))
            result = subprocess.run(f"lli main.ll".split(), stdout=subprocess.PIPE)
        result = result.stdout.decode("utf-8") 

        with open(file_ans_loc, 'r') as f:
            ans = f.read()
            if not ans.strip() == result.strip():
                raise Exception(f"Incorrect output on test case: {file_loc}\nOutput: {result}Expected output: {ans}")
print(f"\nPassed {ctr}/{ctr} test cases")