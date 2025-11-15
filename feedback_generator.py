import anthropic
import json
import pandas as pd
import os
from dotenv import load_dotenv
from tqdm import tqdm
import time

# Load environment variables
load_dotenv()
client = anthropic.Client(api_key=os.getenv('ANTHROPIC_API_KEY'))

# Constants for PALS0045
PALS0045_QUESTION = (
    "Q8. Now, categorise children scoring below the 15th percentile as 'Low'. "
    "You can create a new variable to indicate if the child is above or under "
    "the 15th percentile."
)

PALS0045_MODEL_ANSWER = (
    "#the 15th centile is at -1.04 sd below the mean, so all z scores < -1.04 "
    "are below the 15th centile, we can check this with qnorm(0.15)\n\n"
    "#mark those less than or equal to -1.04 with a 1, and all the others with a zero.\n"
    "irr <- irr %>%\n"
    "  mutate(low_parent=ifelse(ptotal_z< -1.04, 1, 0),\n"
    "         low_teacher=ifelse(ttotal_z< -1.04, 1, 0))"
)

# Constants for PALS0046
PALS0046_QUESTION = (
    "Q8. Calculate the chi-square test of association between return rate "
    "and child language status."
)

PALS0046_MODEL_ANSWER = (
    "# The proportion of parents who returned their questionnaires was associated "
    "with child language status, in that significantly fewer parents of children "
    "with DLD returned their questionnaires (56%) vs those with typically "
    "developing language (83%), x2 (1, N = 200) = 17.20, p < .001, V = 0.29"
)


def generate_ai_feedback(student_answer, question, model_answer, retries=3):
    """Generate AI feedback using Claude with retries"""
    prompt = (
        "You are an excellent instructor teaching a statistics course.\n"
        f"You gave the students the following question in the assignment: {question}\n\n"
        f"The student submission was {student_answer}. "
        f"The model answer is {model_answer}\n\n"
        "Please evaluate the student's submission and provide elaborated "
        "formative feedback.\n"
        "The feedback should be addressed directly to the student as is.\n"
        "It should be no more than 3 lines."
    )

    for attempt in range(retries):
        try:
            response = client.messages.create(
                model="claude-3-5-sonnet-20240620",
                max_tokens=1024,
                temperature=0.7,
                system="You are an expert statistics instructor providing feedback to students.",
                messages=[{"role": "user", "content": prompt}]
            )
            return response.content[0].text
        except Exception as e:
            print(f"Error generating AI feedback (attempt {attempt + 1}/{retries}): {e}")
            if attempt < retries - 1:
                time.sleep(1)
            else:
                return ""


def generate_hybrid_feedback(student_answer, ta_feedback, question, model_answer, retries=3):
    """Generate hybrid feedback using Claude with retries"""
    prompt = (
        "You are an excellent instructor teaching a statistics course.\n"
        f"You gave the students the following question in the assignment: {question}\n\n"
        f"The student submission was {student_answer}. "
        f"The model answer is {model_answer}\n\n"
        f"Here is an instructor's feedback: {ta_feedback}\n\n"
        "Please evaluate the student's submission and the instructor's feedback. "
        "If you believe the instructor's feedback could be improved,\n"
        "provide an improved version based on the current feedback. "
        "The feedback should be addressed directly to the student as is.\n"
        "It should be no more than 3 lines.\n"
        "If you believe the instructor's feedback does not need improvement, "
        "return the original feedback."
    )

    for attempt in range(retries):
        try:
            response = client.messages.create(
                model="claude-3-5-sonnet-20240620",
                max_tokens=1024,
                temperature=0.7,
                system="You are an expert statistics instructor providing feedback to students.",
                messages=[{"role": "user", "content": prompt}]
            )
            return response.content[0].text
        except Exception as e:
            print(f"Error generating hybrid feedback (attempt {attempt + 1}/{retries}): {e}")
            if attempt < retries - 1:
                time.sleep(1)
            else:
                return ""


def should_process_student(student_id, human_feedback):
    """Determine if a student's submission should be processed"""
    if student_id in ["Week1", "MISSING_ID"]:
        return False
    if pd.isna(human_feedback):
        return False
    return True


def create_student_entry(student_answer, question, ai_feedback, human_feedback, hybrid_feedback):
    """Create a dictionary containing student entry data"""
    return {
        "TaskName": "Quiz 1: Exercise 8",
        "TaskBody": question,
        "TaskSolutionBody": student_answer,
        "AIFeedback": ai_feedback,
        "HumanFeedback": human_feedback,
        "HumanPlusAIFeedback": hybrid_feedback
    }


def process_course_feedback(csv_file, course_code, question, model_answer):
    """Process submissions for a specific course"""
    try:
        df = pd.read_csv(csv_file, encoding='iso-8859-1')
    except Exception as e:
        print(f"Error reading CSV file {csv_file}: {e}")
        return {}

    valid_submissions = df[df.apply(
        lambda x: should_process_student(x['StudentID'], x['HumanFeedback']),
        axis=1
    )]

    print(f"\nProcessing {course_code}")
    print(f"Found {len(valid_submissions)} valid submissions to process")

    course_data = {}

    for _, row in tqdm(valid_submissions.iterrows(),
                       total=len(valid_submissions),
                       desc=f"Processing {course_code} submissions"):
        student_id = str(row['StudentID']).upper()
        student_answer = row['TaskSolutionBody']
        human_feedback = row['HumanFeedback']

        print(f"\nProcessing student {student_id}")
        ai_feedback = generate_ai_feedback(student_answer, question, model_answer)
        if ai_feedback:
            print(f"Generated AI feedback for {student_id}")

        hybrid_feedback = generate_hybrid_feedback(
            student_answer, human_feedback, question, model_answer
        )
        if hybrid_feedback:
            print(f"Generated hybrid feedback for {student_id}")

        if ai_feedback and hybrid_feedback:
            course_data[student_id] = create_student_entry(
                student_answer, question, ai_feedback,
                human_feedback, hybrid_feedback
            )

            # Save progress
            try:
                output_data = {
                    "PALS0045": {},
                    "PALS0046": {}
                }
                output_data[course_code] = course_data

                with open('feedback_output.txt', 'w', encoding='utf-8') as f:
                    json.dump(output_data, f, indent=4, ensure_ascii=False)
                print(f"Saved progress for {student_id}")
            except Exception as e:
                print(f"Error saving progress for {student_id}: {e}")

    return course_data


def process_all_feedback():
    """Process feedback for all courses"""
    # Process both courses
    output_data = {
        "PALS0045": process_course_feedback(
            'student_tasks_with_feedback.csv',
            'PALS0045',
            PALS0045_QUESTION,
            PALS0045_MODEL_ANSWER
        ),
        "PALS0046": process_course_feedback(
            'firstyear_student_tasks_with_feedback.csv',
            'PALS0046',
            PALS0046_QUESTION,
            PALS0046_MODEL_ANSWER
        )
    }

    # Save final results
    try:
        with open('feedback_output.txt', 'w', encoding='utf-8') as f:
            json.dump(output_data, f, indent=4, ensure_ascii=False)
        print("\nFinal results saved successfully")
    except Exception as e:
        print(f"\nError saving final results: {e}")

    print("\nProcessing complete.")
    print(f"PALS0045: Processed {len(output_data['PALS0045'])} submissions successfully")
    print(f"PALS0046: Processed {len(output_data['PALS0046'])} submissions successfully")


if __name__ == "__main__":
    process_all_feedback()
