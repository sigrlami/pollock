import { IEditablePoll } from '../../shared/types/app';

interface IReduxState {
  newPoll: IEditablePoll;
  communication: {
    pollCreating: {
      isRequesting: boolean;
      error: string;
    };
  };
  questionsTemplates: IQuestionTemplate[];
}

interface IQuestionTemplate {
  text: string;
  answers: string[];
}

export { IReduxState, IQuestionTemplate };
