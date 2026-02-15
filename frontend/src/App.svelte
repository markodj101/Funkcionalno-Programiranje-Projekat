<script lang="ts">
  import { RotateCcw } from "lucide-svelte";
  import './app.css';


  interface Memory {
    [key: string]: number; 
  }

  interface ExecutionStep {
    pc: number; 
    instruction: string; 
    memorySnapshot: Memory; 
  }

  interface ServerResponse {
    steps: ExecutionStep[]; 
    error?: string; 
  }


  let inputValue: string = "";
  let memoryInput: string = "";
  let statusMessage = "Ready";
  let currentPC = 0;
  
  
  let playbackTimer: any = null;
  
  let logs: ExecutionStep[] = [];
  let runtimeMemory: Memory = {};
  let delta: number = 500;
  let isDragging = false;

 

  async function sendToHaskell() {
    if (!inputValue) return;

    resetState();
    statusMessage = "Processing...";

    try {
      const res = await fetch("http://localhost:3000/api/run", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        
        body: JSON.stringify({ 
            sourceCode: inputValue, 
            initialMemory: memoryInput
        }),
      });

      if (res.ok) {
        const data: ServerResponse = await res.json();
        console.log("Haskell Trace:", data);
        
        if (data.error){
          statusMessage = "Error: " + data.error;
        } else {
          animateExecution(data.steps);
        }
      } else {
          statusMessage = "Server Error: " + res.status;
      }
    } catch (err) {
      console.error(err);
      statusMessage = "Connection failed.";
    }
  }

  function animateExecution(steps: ExecutionStep[]){
    let index = 0;
    
    statusMessage = `Running (${steps.length} steps)...`;

    if (playbackTimer) clearInterval(playbackTimer);

    playbackTimer = setInterval(() => {
      if (index >= steps.length) {
        clearInterval(playbackTimer);
        statusMessage = "Execution finished.";
        return;
      }

      const step = steps[index];

     
      logs = [...logs, step];
      runtimeMemory = step.memorySnapshot;
      currentPC = step.pc;

      
      const consoleDiv = document.getElementById("console-output");
      if (consoleDiv) consoleDiv.scrollTop = consoleDiv.scrollHeight;

      index++;
    }, delta);
  }

  function resetState(){
    if (playbackTimer) clearInterval(playbackTimer);
    logs = [];
    runtimeMemory = {};
    currentPC = 0;
    statusMessage = "Ready";
  }


  function handleDrop(event: DragEvent){
    event.preventDefault();
    isDragging = false;

    const file = event.dataTransfer?.files[0];
    if (file && (file.name.endsWith(".asm") || file.name.endsWith(".txt"))) {
      const reader = new FileReader();
      reader.onload = (e) => {
        inputValue = e.target?.result as string;
      };
      reader.readAsText(file);
    } else {
      alert("Podržani su samo .asm i .txt fajlovi.");
    }
  }

  function handleDragOver(event: DragEvent){
    event.preventDefault();
    isDragging = true;
  }

  function handleDragLeave(){
    isDragging = false;
  }
</script>


<main class="min-h-screen flex flex-col h-screen bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100 font-sans">
  
  
  <header class="bg-white dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 shrink-0">
    <div class="w-full max-w-6xl mx-auto px-4 py-4 flex items-center justify-between">
      <div class="flex items-center gap-3">
        <p class="text-2xl font-bold leading-tight ">Recursive Haskell Assembler</p>
      </div>

      <div class="flex items-center gap-4">
        
        <div class="flex items-center gap-2 px-4 py-2 bg-gray-100 dark:bg-gray-700 rounded-full border border-gray-200 dark:border-gray-600">
          <span class="text-xs font-semibold text-gray-500 dark:text-gray-300 uppercase tracking-wider">Delay:</span>
          <input
            type="range" min="0" max="2000" step="50"
            bind:value={delta}
            class="w-32 h-1 accent-blue-500 cursor-pointer"
          />
          <span class="text-xs font-bold text-gray-700 dark:text-gray-100 w-12 text-right">{delta} ms</span>
        </div>

        
        <button on:click={sendToHaskell} class="flex items-center gap-2 px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 shadow-md transition-all active:scale-95 font-semibold">
          Run
        </button>

        <button on:click={resetState} class="p-2 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-700 transition-colors">
          <RotateCcw size={20} />
        </button>
      </div>
    </div>
  </header>

  
  <section class="flex-1 w-full max-w-6xl mx-auto p-4 overflow-hidden h-full">
    <div class="flex gap-4 h-full">
    
    
      <div class="w-1/3 flex flex-col gap-4 h-full">
       
        <!-- svelte-ignore a11y_no_static_element_interactions -->
        <div 
          class="flex-1 bg-white dark:bg-gray-800 rounded-xl border border-gray-300 dark:border-gray-700 flex flex-col shadow-sm relative overflow-hidden group"
          on:dragover={handleDragOver}
          on:dragleave={handleDragLeave}
          on:drop={handleDrop}
        >
          <div class="p-3 border-b border-gray-200 dark:border-gray-700 font-bold flex justify-between uppercase text-xs text-gray-500 bg-gray-50 dark:bg-gray-800">
             <span>Assembly Code</span>
             <span class="opacity-50">.txt / .asm</span>
          </div>

          {#if isDragging}
            <div class="absolute inset-0 z-10 bg-blue-500/20 backdrop-blur-sm flex items-center justify-center pointer-events-none">
              <p class="text-blue-600 bg-white px-4 py-2 rounded-lg shadow-lg font-bold animate-pulse">
                Drop file here...
              </p>
            </div>
          {/if}

          <textarea 
            class="flex-1 p-4 font-mono bg-transparent outline-none resize-none text-sm leading-relaxed" 
            bind:value={inputValue}
            placeholder="Type code here..."
            spellcheck="false"
          ></textarea>
        </div>

        
        <div class="h-1/3 bg-white dark:bg-gray-800 rounded-xl border border-gray-300 dark:border-gray-700 flex flex-col shadow-sm overflow-hidden">
          <div class="p-3 border-b border-gray-200 dark:border-gray-700 font-bold uppercase text-xs text-gray-500 bg-gray-50 dark:bg-gray-800">
             Initial Memory
          </div>
          <textarea 
             class="flex-1 p-4 font-mono bg-transparent outline-none resize-none text-sm" 
             placeholder="result 0" 
             bind:value={memoryInput} 
             spellcheck="false"
          ></textarea>
        </div>
      </div>

     
      <div class="flex-1 flex flex-col gap-4 h-full">
        
        
        <div class="h-1/3 bg-white dark:bg-gray-800 border-gray-300 dark:border-gray-700 rounded-xl border shadow-sm relative overflow-hidden flex flex-col">
          <div class="p-3 border-b border-gray-200 dark:border-gray-700 font-bold uppercase text-xs text-gray-500 flex justify-between bg-gray-50 dark:bg-gray-800 items-center">
             <span>Runtime Memory State</span>
             <span class="text-xs bg-blue-100 text-blue-700 px-2 py-0.5 rounded border border-blue-200 font-mono">PC: {currentPC}</span>
          </div>
          
          <div class="flex-1 p-4 overflow-auto bg-gray-50/50 dark:bg-black/20">
            {#if Object.keys(runtimeMemory).length === 0}
              <div class="h-full flex items-center justify-center text-gray-400 italic text-sm">Memory is empty.</div>
            {:else}
             
              <div class="grid grid-cols-4 gap-3">
                {#each Object.entries(runtimeMemory) as [key, value]}
                  <div class="flex flex-col items-center p-2 bg-white dark:bg-gray-700 rounded border border-gray-200 dark:border-gray-600 shadow-sm">
                      <span class="text-[10px] text-gray-400 uppercase font-bold">{key}</span>
                      <span class="text-lg font-mono font-bold text-blue-600 dark:text-blue-400">{value}</span>
                  </div>
                {/each}
              </div>
            {/if}
          </div>
        </div>

        
        <div class="flex-1 bg-[#0d1117] rounded-xl border border-gray-700 shadow-lg flex flex-col overflow-hidden">
          <div class="px-4 py-2 border-b border-gray-800 bg-gray-800/50 flex justify-between items-center">
              <span class="text-xs font-bold text-orange-400 uppercase tracking-wider">Terminal Trace</span>
              <span class="text-[10px] text-gray-500">{statusMessage}</span>
          </div>
          
          <div id="console-output" class="flex-1 p-4 font-mono text-sm overflow-y-auto space-y-1">
              {#if logs.length === 0}
                  <span class="opacity-30 text-gray-400 italic">Ready for execution...</span>
              {:else}
                  {#each logs as log}
                      <div class="flex gap-3 text-gray-300 border-l-2 border-transparent hover:border-blue-500 pl-2 transition-all">
                          <span class="text-gray-600 text-[10px] w-8 text-right pt-1 opacity-50">[{log.pc}]</span>
                          <span class="text-yellow-500 font-bold">{log.instruction}</span>
                      </div>
                  {/each}
              {/if}
          </div>
        </div>
      </div>

    </div>
  </section>
</main>