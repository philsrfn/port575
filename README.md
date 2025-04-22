# Haiku Quiz API 🌸❓

A minimalist web server built with Haskell and the Scotty web framework that serves quiz questions in haiku form. I mainly built this to learn about Scotty!

Access it on [https://haiku.philhrtm.xyz](https://haiku.philhrtm.xyz/)

Designed as a pure JSON API, but includes a basic check to serve HTML instructions if accessed via a standard web browser.

Notably, the core server logic in `app/Main.hs` is kept under 100 lines of code.

## Features

*   Serves random haiku questions.
*   Accepts answers via POST request.
*   Responds with a success or failure haiku.
*   Simple in-memory state for the current question.
*   Serves basic instructions to web browsers.

## Setup and Running

1.  **Prerequisites:**
    *   GHC (Haskell Compiler)
    *   Cabal (Build Tool)
2.  **Build:**
    ```bash
    cabal build executable:Port575
    ```
3.  **Run:**
    ```bash
    cabal run Port575
    ```
    The server will start on `http://localhost:3000`.

## API Usage

Use `curl` or any other API client.

*   **Welcome Message:**
    ```bash
    curl http://localhost:3000/
    # Response: {"message":"Welcome to the Haiku Quiz! GET /question for a challenge."}
    ```
*   **Get a Question:**
    ```bash
    curl http://localhost:3000/question
    # Response: {"haiku":["Line one of haiku","Line two of haiku","Line three of haiku"]}
    ```
*   **Submit an Answer:**
    ```bash
    curl -X POST -H "Content-Type: application/json" -d '{"submittedAnswer": "YourGuess"}' http://localhost:3000/answer
    # Response (Correct): {"result":["Success haiku line one","Success haiku line two","Success haiku line three"]}
    # Response (Incorrect): {"result":["Failure haiku line one","Failure haiku line two","Failure haiku line three"]}
    ```
*   **Reset State:**
    ```bash
    curl -X POST http://localhost:3000/reset
    # Response: {"message":"Quiz state reset."}
    ```

## Browser Access

Accessing the endpoints directly in a web browser will display a simple HTML page with instructions, as the API is intended.
