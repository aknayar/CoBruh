import os
import subprocess

# all the test files are in /tests
directory = os.fsencode("./tests")
    
for file in os.listdir(directory):
    # run the file
    # get the output
    # compare with corresponding .err or .out file
    # if it doens't match, throw an error describing what went wrong
    filename = os.fsdecode(file)
    if filename.endswith(".err"): 
        file_ans_loc = os.path.join("./tests/", filename)
        file_loc = os.path.join("./tests/", '.'.join(filename.split('.')[:-1] + ["bruh"]))
  
        result = subprocess.run(f"dune exec -- CoBruh -c {file_loc}".split(), stdout=subprocess.PIPE)
        result = result.stdout.decode("utf-8") 

        ans = None
        with open(file_ans_loc, 'r') as f:
            ans = f.read()
        
        if not ans.strip() == result.strip():
            raise Exception(f"Incorrect output on test case: {file_loc}\nOutput: {result}Expected output: {ans}")

    if filename.endswith(".out"):
        file_ans_loc = os.path.join("./tests/", filename)
        file_loc = os.path.join("./tests/", '.'.join(filename.split('.')[:-1] + ["bruh"]))
  
        ir_result = subprocess.run(f"dune exec -- CoBruh -c {file_loc}".split(), stdout=subprocess.PIPE)
        with open("main.ll", 'w') as writer:
            writer.write(ir_result.stdout.decode("utf-8"))
        result = subprocess.run(f"lli main.ll".split(), stdout=subprocess.PIPE)
        result = result.stdout.decode("utf-8") 

        ans = None
        with open(file_ans_loc, 'r') as f:
            ans = f.read()
      
        if not ans.strip() == result.strip():
            raise Exception(f"Incorrect output on test case: {file_loc}\nOutput: {result}Expected output: {ans}")