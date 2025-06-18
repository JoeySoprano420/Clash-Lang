import asyncio
import websockets
import json

USERS = set()

async def handler(websocket):
    USERS.add(websocket)
    try:
        async for message in websocket:
            data = json.loads(message)
            # Broadcast to all others
            for user in USERS:
                if user != websocket:
                    await user.send(json.dumps(data))
    finally:
        USERS.remove(websocket)

async def main():
    async with websockets.serve(handler, "0.0.0.0", 8765):
        print("🌐 Clashup Collaboration Server running on ws://localhost:8765")
        await asyncio.Future()  # Run forever

asyncio.run(main())
