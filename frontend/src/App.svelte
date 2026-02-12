<script lang="ts">
import {Activity,Play,RotateCcw,FileCode,Database,Terminal} from "lucide-svelte";
import './app.css';
interface Memory {
  [key: string]: number; // npr. { "x": 10, "y": 20 }
}

interface ExecutionStep {
  pc: number; // Na kojoj smo liniji koda (0, 1, 2...)
  instruction: string; // Tekst instrukcije (npr. "LOAD x 10")
  memorySnapshot: Memory; // Kako izgleda memorija NAKON te instrukcije
}

interface ServerResponse {
  steps: ExecutionStep[]; // Lista svih koraka (naš "film")
  error?: string; // Ako se desi greška (npr. dijeljenje s nulom)
}



  let inputValue: string = "";
  let memoryInput: string = "";
  let serverMessage: string = "";
  let luckyNumber: number | null = null;

  // UI state for header controls
  let delta: number = 500;
  function setDelta(n: number) {
    delta = n;
  }

  async function sendToHaskell() {
    if (!inputValue) return;

    try {
      const res = await fetch("http://localhost:3000/api/log", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ message: inputValue }),
      });

      if (res.ok) {
        // Čitamo odgovor
        const data = await res.json();
        
        console.log("Stiglo od servera:", data);
        
        // Spremamo podatke u varijable za prikaz
        serverMessage = data.reply;
        luckyNumber = data.magicNumber; 
      } 
    } catch (err) {
      console.error(err);
      serverMessage = "Greška u komunikaciji.";
    }
  }

  let isDragging = false;

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
    }else {
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

<main class="min-h-screen flex flex-col h-screen bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
  <header class="bg-white dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700">
    <div class="w-full max-w-6xl mx-auto px-4 py-4 flex items-center justify-between">
      <div class="flex items-center gap-3">
        <div>
          <p class="text-4xl font-bold leading-tight">Recursive Haskell Assembler</p>
        </div>
      </div>

      <div class="flex items-center gap-4">
        <div class="flex items-center gap-2 px-4 py-2 bg-gray-100 dark:bg-gray-700 rounded-full">
          <span class="text-xs font-semibold text-gray-500 dark:text-gray-200 uppercase tracking-wider">Delta (Delay):</span>
          <input
            type="range"
            min="0"
            max="2000"
            step="50"
            bind:value={delta}
            on:input={(e) => setDelta(Number((e.target as HTMLInputElement).value))}
            class=" w-32 accent-blue-600 dark:accent-blue-500 cursor-pointer range-sm"
          />
          <span class="text-xs font-bold text-gray-700 dark:text-gray-100 w-12 text-right">{delta} ms</span>
        </div>

        <button on:click={sendToHaskell} class="flex items-center gap-2 px-4 py-2 bg-white text-black rounded-lg hover:bg-gray-600 dark:bg-gray-700 dark:text-gray-100 transition-colors">
          Pošalji
        </button>

        <button >
          <RotateCcw size={18} />
        </button>
      </div>
    </div>
  </header>

  <section class="flex-1 w-full  ">
    <div class="flex-1 grid grid-cols-12 gap-4 p-4 overflow-hidden">
    
<div class="col-span-4 flex flex-col gap-4 py-4 overflow-hidden h-[800px]">
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div 
    class="flex-1 bg-white rounded-xl border border-gray-500 flex flex-col shadow-sm dark:bg-gray-800 relative transition-all"
    on:dragover={handleDragOver}
    on:dragleave={handleDragLeave}
    on:drop={handleDrop}
  >
    <div class="p-3 border-b border-gray-500 font-bold flex justify-between uppercase text-xs text-white bg-gray-800 rounded-t-xl">
       <span>Assembly Code</span>
       <span class="text-[10px] opacity-50">.txt / .asm</span>
    </div>

    {#if isDragging}
      <div class="absolute inset-0 z-10 bg-blue-500/20 backdrop-blur-sm rounded-xl flex items-center justify-center pointer-events-none">
        <p class="text-orange-500 text-xl font-bold  px-4 py-2 rounded-lg  animate-pulse">
          Drop file here...
        </p>
      </div>
    {/if}

    <textarea 
      class="flex-1 p-4 font-mono bg-transparent outline-none resize-none" 
      bind:value={inputValue}
      placeholder="Type code here..."
    ></textarea>
  </div>
      <div class="h-1/3 bg-white rounded-xl border border-gray-500 flex flex-col shadow-sm dark:bg-gray-800">
        <div class="p-3 border-b border-gray-500 font-bold uppercase text-xs text-gray-500 text-white">
           Initial Memory State
        </div>
        <textarea class="flex-1 p-4 font-mono bg-transparent outline-none resize-none" placeholder="result 0" bind:value={memoryInput}></textarea>
      </div>
    </div>

    <div class="col-span-8 flex flex-col gap-4   py-4 overflow-hidden">
      <div class="flex-1 bg-white border-gray-500 rounded-xl border border-gray-200 shadow-sm relative dark:bg-gray-800">
        <div class="p-3 border-b border-gray-500 font-bold uppercase text-xs text-gray-500 flex justify-between text-white">
           <span>Runtime Memory State</span>
        </div>
        <div class="flex items-center justify-center h-full italic text-gray-400">
          Memory is empty. Initialize above.
        </div>
      </div>

      <div class="h-1/2 bg-[#0d1117] rounded-xl border border-gray-800 shadow-lg flex flex-col">
        <div class="p-3 border-b border-gray-800 font-bold uppercase text-xs text-orange-400 flex items-center gap-2 ">
           <span> Execution Trace Console</span>
        </div>
        <div class="flex-1 p-4 font-mono text-gray-300 overflow-y-auto">
          <span class="opacity-50 italic">Waiting for execution...</span>
        </div>
      </div>
    </div>

  </div>
  </section>

  {#if luckyNumber !== null}
    <div class="result p-4 max-w-md mx-auto">
      <p class="mb-2">Status: {serverMessage}</p>
      <h2 class="text-lg font-semibold">Tvoj sretni broj je: {luckyNumber}</h2>
    </div>
  {/if}
</main>

