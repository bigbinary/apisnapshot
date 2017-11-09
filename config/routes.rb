Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
  resources :api_responses, only: [:show, :create]

  root 'home#index'
end
