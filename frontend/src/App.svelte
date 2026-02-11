<script lang="ts">
  let inputValue: string = "";
  let serverMessage: string = "";
  let luckyNumber: number | null = null; // Varijabla za broj

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
</script>

<main>
  <h1>Haskell Random Generator</h1>

  <div class="card">
    <input type="text" bind:value={inputValue} placeholder="Upiši nešto..." />
    <button on:click={sendToHaskell}>Pošalji</button>
  </div>

  {#if luckyNumber !== null}
    <div class="result">
      <p>Status: {serverMessage}</p>
      <h2>Tvoj sretni broj je: {luckyNumber}</h2>
    </div>
  {/if}
</main>

<style>
  .result {
    margin-top: 20px;
    padding: 10px;
    background-color: #f0f0f0;
    border-radius: 8px;
    color: #333;
  }
</style>