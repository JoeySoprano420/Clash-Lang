import asyncio, websockets

clients = set()
async def echo(websocket, path):
    clients.add(websocket)
    try:
        async for msg in websocket:
            for client in clients:
                if client != websocket:
                    await client.send(msg)
    finally:
        clients.remove(websocket)
async def main():
    async with websockets.serve(echo, "0.0.0.0", 9898):
        print("Arena Server Live!")
        await asyncio.Future()
asyncio.run(main())
