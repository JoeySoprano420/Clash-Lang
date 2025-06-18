import json

LESSONS = [
    {"id": 1, "title": "💡 Variables", "goal": "let x = 5", "badge": "🟢"},
    {"id": 2, "title": "📥 Input", "goal": "input(name)", "badge": "🟡"},
    {"id": 3, "title": "📤 Print", "goal": "print(name)", "badge": "🟠"},
    {"id": 4, "title": "🧠 Conditionals", "goal": "if_eq x 7", "badge": "🔵"}
]

SAVE_FILE = "academy_progress.json"

def load_progress():
    try:
        with open(SAVE_FILE) as f:
            return json.load(f)
    except:
        return {}

def save_progress(data):
    with open(SAVE_FILE, 'w') as f:
        json.dump(data, f, indent=2)

def run_lesson(lesson_id):
    lesson = LESSONS[lesson_id - 1]
    print(f"\n📘 Lesson {lesson_id}: {lesson['title']}")
    print("Try typing:\n", lesson["goal"])
    code = input("🧪 Your code: ").strip()
    if lesson["goal"] in code:
        print(f"✅ Success! Badge earned: {lesson['badge']}")
        progress = load_progress()
        progress[str(lesson_id)] = True
        save_progress(progress)
    else:
        print("❌ Incorrect — try again.")

def show_menu():
    print("🎓 Clashup Academy")
    progress = load_progress()
    for lesson in LESSONS:
        status = lesson["badge"] if str(lesson["id"]) in progress else "🔒"
        print(f"{lesson['id']}. {lesson['title']} [{status}]")

    choice = input("Pick lesson number: ")
    if choice.isdigit():
        run_lesson(int(choice))

if __name__ == "__main__":
    show_menu()
