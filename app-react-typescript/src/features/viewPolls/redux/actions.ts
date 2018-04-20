import { IExtraArguments, IReduxState, IPoll, IAction, IPollResult, IAnswer } from 'shared/types/app';
import Dispatch = Redux.Dispatch;

function loadPolls() {
  return async(
    dispatch: Dispatch<any>, getState: () => IReduxState, { api }: IExtraArguments
  ) => {
    dispatch({ type: 'VIEW_POLLS:LOAD_POLLS' });

    try {
      const polls = api.loadPolls();
      dispatch({ type: 'VIEW_POLLS:LOAD_POLLS_SUCCESS', payload: polls });
    } catch (error) {
      dispatch({ type: 'VIEW_POLLS:LOAD_POLLS_SUCCESS', payload: error.message });
    }
  };
}

function loadPoll(id: string) {
  return async(
    dispatch: Dispatch<any>, getState: () => IReduxState, { api }: IExtraArguments
  ) => {
    dispatch({ type: 'VIEW_POLLS:LOAD_POLL' });

    try {
      const poll: IPoll | null = api.loadPoll(id);
      const results: IPollResult[] = api.loadPollResults(id);
      dispatch({ type: 'VIEW_POLLS:LOAD_POLL_SUCCESS', payload: { poll, results } });
    } catch (error) {
      dispatch({ type: 'VIEW_POLLS:LOAD_POLL_FAIL', payload: error.message });
    }
  };
}

function saveAnswers() {
  return async(
    dispatch: Dispatch<any>, getState: () => IReduxState, { api }: IExtraArguments
  ) => {
    dispatch({ type: 'VIEW_POLLS:SAVE_ANSWERS' });
    const selectedPoll = getState().viewPolls.selectedPoll;
    const poll: IPoll | null = selectedPoll ? selectedPoll.poll : null;

    if (!poll) {
      return dispatch({ type: 'VIEW_POLLS:SAVE_ANSWERS_FAIL', payload: 'Poll is not loaded' });
    }

    const result: IPollResult = {
      pollId: poll.id.toString(),
      questions: poll.questions.map(question => {
        const selectedAnswer = question.answers.find((answer: IAnswer) => answer.isSelected);
        return {
          text: question.text,
          answerId: selectedAnswer ? selectedAnswer.id : '',
          answer: selectedAnswer ? selectedAnswer.text : '',
        };
      }),
    };

    try {
      api.savePollResult(result);
      dispatch({ type: 'VIEW_POLLS:SAVE_ANSWERS_SUCCESS' });
    } catch (error) {
      dispatch({ type: 'VIEW_POLLS:SAVE_ANSWERS_FAIL', payload: error.message });
    }
  };
}

function selectAnswer(questionIndex: number, answerIndex: number): IAction {
  return { type: 'VIEW_POLLS:SELECT_ANSWER', payload: { questionIndex, answerIndex } };
}

export {
  selectAnswer,
  saveAnswers,
  loadPolls,
  loadPoll,
}
