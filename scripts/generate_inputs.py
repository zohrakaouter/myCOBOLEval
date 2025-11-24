import ast
import builtins
import json
import os
import re
import textwrap
from typing import Tuple, Any, List
import inspect

from openai import OpenAI
from pydantic import BaseModel, Field
from openai.types.chat import ChatCompletionMessageParam
from openai.types.chat.completion_create_params import ResponseFormatJSONSchema # noqa
from pydantic.v1 import ValidationError

from scripts.utils import Model

PROMPT=""

class InputsOutputsDataCBL(BaseModel):
    pair_input_output:List[Tuple[Any,Any]]= Field(
        description= "The generated inputs and their respectful outputs of the COBOL program"
    )




def load_cbl_program(path:str):
    try:
        with open(path,"r") as f:
            cobol_file_content=f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"File {path} not found")
    except Exception as e:
        raise Exception(f"Exception in loading {path}: {e}")
    return cobol_file_content

def generate_inputs(messages, model):
    response = client.chat.completions.create(
        model=model.name,
        messages=messages,
        # temperature=model.temp,
        temperature=0.2,

    )
    return response.choices[0].message.content

def parse_model_response(raw_response: str) -> InputsOutputsDataCBL:
    """
    Extract JSON from LLM output and parse into Pydantic model.
    """
    # Remove markdown code fences
    cleaned = re.sub(r"^```(json)?", "", raw_response.strip(), flags=re.IGNORECASE)
    cleaned = re.sub(r"```$", "", cleaned).strip()

    # Extract JSON object if there is extra text
    match = re.search(r"\{[\s\S]*\}", cleaned)
    if match:
        cleaned = match.group(0)

    try:
        data = json.loads(cleaned)
        return InputsOutputsDataCBL(**data)
    except json.JSONDecodeError as e:
        print("⚠️ JSON decoding failed:", e)
        print("Raw output:\n", raw_response)
        raise
    except ValidationError as e:
        print("⚠️ Validation failed:", e)
        raise
def save_to_json_file(parsed_data: InputsOutputsDataCBL, output_path: str):
    """
    Save the Pydantic model to a JSON file using Pydantic v2 method.
    """
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(parsed_data.model_dump(), f, indent=4, ensure_ascii=False)
    print(f"✅ Saved structured output to {output_path}")
def get_function_from_json(file_path, entry_point):
    """
    Extracts a Python function definition from a JSONL file given an entry_point name.
    """
    with open(file_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            data = json.loads(line)
            if data.get("entry_point") == entry_point:
                # Extract the canonical solution
                func_body = data["canonical_solution"].rstrip("\n")
                return f"def {entry_point}(*args, **kwargs):\n" + "\n".join("    " + line for line in func_body.splitlines())
    return None
def get_canonical_solution_from_json(file_path, entry_point):
    """
    Extracts a Python function definition from a JSONL file given an entry_point name.
    """
    with open(file_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            data = json.loads(line)
            if data.get("entry_point") == entry_point:
                # Extract the canonical solution
                return data["canonical_solution"].rstrip("\n")




def infer_function_signature(entry_point, canonical_solution):
    # Remove extra indentation
    canonical_solution = textwrap.dedent(canonical_solution).strip("\n")

    # Parse the canonical solution into an AST
    tree = ast.parse(canonical_solution)

    # Collect all variable names used
    used_names = {n.id for n in ast.walk(tree) if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load)}

    # Collect locally defined names (assignments, loop vars, comprehension vars, function args)
    local_names = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.arg):
            local_names.add(node.arg)
        elif isinstance(node, ast.Name) and isinstance(node.ctx, ast.Store):
            local_names.add(node.id)
        elif isinstance(node, ast.comprehension):
            if isinstance(node.target, ast.Name):
                local_names.add(node.target.id)
        elif isinstance(node, (ast.FunctionDef, ast.ClassDef)):
            local_names.add(node.name)

    # Filter out defined names and Python built-ins
    builtin_names = set(dir(builtins))
    param_candidates = [n for n in used_names if n not in local_names and n not in builtin_names]

    # Sort parameters for consistency
    params = ", ".join(sorted(param_candidates))

    # Build the function string
    func_def = f"def {entry_point}({params}):\n"
    func_def += textwrap.indent(canonical_solution, "    ")
    return func_def

def check_input_output(entry_point,canonical_solution, input,output):
    namespace = {}
    exec(infer_function_signature(entry_point, canonical_solution), namespace)
    #print("in checking input output ", namespace[entry_point](input))
    #print("in checking input output ",  run_entry_point(namespace,entry_point,input))
    canonical_running=run_entry_point(namespace,entry_point,input)
    print("after canonical running ", canonical_running)

    if output==canonical_running:

        print(input,"input output is correct",output)
    else:
        print(input, "   input output is incorrect  ",output)



    #print("the canonical ", entry_point, "resulting output is ", namespace["add"](3, 4))



def run_entry_point(namespace, entry_point, input_data):
    """
    Safely call a function from namespace[entry_point] with input_data,
    regardless of number of parameters.

    ```
    namespace: dict containing functions
    entry_point: str, function name
    input_data: list, tuple, or dict with input values
    """
    if entry_point not in namespace:
        raise ValueError(f"Function '{entry_point}' not found in namespace")

    func = namespace[entry_point]
    sig = inspect.signature(func)
    params = sig.parameters
    #print(" the params are ", params)

    # If input is a dict, match by parameter names
    #print( "the type of my input is ", input_data," its type is  ",type(input_data))
    if isinstance(input_data, (list, tuple)) and len(params) == 1: input_data = (input_data,)

    if isinstance(input_data, dict):
        args = {k: input_data[k] for k in params if k in input_data}
        #print(" the args are ", args)
        return func(**args)
    elif isinstance(input_data, (list, tuple)):  # Match input_data by position
        return func(*input_data[:len(params)])

    else:  # assume list/tuple
        return func(*input_data[:len(params)])

# Example usage


if __name__ == "__main__":

    specsCOBOLEval=""
    with open("../data/CobolEval.jsonl", "r") as f:
        specsCOBOLEval= [json.loads(line) for line in f.readlines()]

    #example_path="/home/zkebaili/Documents/Travail/Inria ingénieur R&D/data/COBOLEval/scripts/preds/openai/gpt-4o/solutions/parse_music.cbl"
    #example_path = "/home/zkebaili/Documents/Travail/Inria ingénieur R&D/data/COBOLEval/scripts/preds/openai/gpt-4o/solutions/strongest_extension.cbl"
    #example_path = "/home/zkebaili/Documents/Travail/Inria ingénieur R&D/data/COBOLEval/scripts/preds/openai/gpt-4o/solutions/solve.cbl"
    #example_path = "/home/zkebaili/Documents/Travail/Inria ingénieur R&D/data/COBOLEval/scripts/preds/openai/gpt-4o/solutions/any_int.cbl"
    example_path = "/home/zkebaili/Documents/Travail/Inria ingénieur R&D/data/COBOLEval/scripts/preds/openai/gpt-4o/solutions/add.cbl"

    cobol_file_content = load_cbl_program(example_path)
    match = re.search(r'PROGRAM-ID\.\s*([A-Z0-9-]+)', cobol_file_content, re.IGNORECASE)
    program_name = match.group(1)
    program_name = re.sub(r'[^A-Za-z]', '', program_name)
    print(program_name)



    PROMPT=""""
Here is a COBOL program: ```{} ```. Give a list of 10 possible different inputs for this COBOL program. Execute the program with each input and give me the output of the program back.
I want the exact output of the cobol program execution with these inputs.
Return the result strictly as JSON in the following format:


{{
  "pair_input_output": [
    [input1, output1={}(input1)],
    [input2, output2={}(input2)]
  ]
}}
"""

    client = OpenAI(
        base_url="https://openrouter.ai/api/v1",
        api_key=os.getenv("OPENROUTER_API_KEY"),
    )
    messages = [
        {
            "role": "system",
            "content": PROMPT.format(cobol_file_content,program_name,program_name),
        }
    ]

    model = Model(name="openai/gpt-4o", samples_per_task=1)

    response=generate_inputs(messages, model)
   # print(PROMPT.format(cobol_file_content,program_name,program_name))
    parsed = parse_model_response(response)

    output_file = "add_ints_output_temp0.2.json"
    save_to_json_file(parsed, output_file)

    # Example usage
    file_path = "../data/CobolEval.jsonl"  # your file path
    entry_point = "add"
    #func_def = get_function_from_json(file_path, entry_point)

    canonical_solution=get_canonical_solution_from_json(file_path, entry_point)
    #print( "CANONICAL SOLUTION:",canonical_solution)

    #print(infer_function_signature(entry_point, canonical_solution))

    #data = json.loads(output_file)

    for pair in parsed.pair_input_output:
        input_value = pair[0]  # this is already a list of numbers
        output_value = pair[1]  # this is a single number
        #print("Input:", input_value, "it is ", type(input_value))
        #print("Output:", output_value)
        check_input_output(entry_point,canonical_solution,input_value,output_value)


    #print(parsed.pair_input_output)



